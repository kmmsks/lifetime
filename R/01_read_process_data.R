
# Read results -----------------------------------------------------------------
# read all excel_sheets to a list of deata.tables
read_sheets <- function(path){
  lst <- path %>% 
    excel_sheets() %>% 
    purrr::set_names() %>% 
    purrr::map(read_excel, path = path)
  lapply(lst, setDT)
  lst
}

## Incidence ----

# xlsx files
files <- here("data") %>% list.files(full.names = T, pattern = ".xlsx")
names(files) <- here("data") %>% list.files(pattern = ".xlsx") %>% word(sep = fixed("."))

# read to list:
incidence_xlsx <- lapply(files, read_sheets)

# fst files
files <- here("data") %>% list.files(full.names = T, pattern = ".fst")
names(files) <- here("data") %>% list.files(pattern = ".fst") %>% word(sep = fixed("."))

# read to list:
incidence_fst <- lapply(files, read_fst)
lapply(incidence_fst, setDT)

# combine xlsx and fst:
datlist_incidence <- c(incidence_xlsx, incidence_fst)

# dgs tolower case for consistancy
datlist_incidence$ci_results_cohort_95_99 %>% rbindlist(idcol = "dg", fill = T) %>% .[, dg := dg %>% tolower()]
datlist_incidence$ci_results_all_specialities  %>% .[, dg := dg %>% tolower()]


## prevalence ----
dat_preval <- fread(here("data", "service_utilization.csv"))

# TABLES -----------------------------------------------------------------------
## Table ci by dg --------------------------------------------------------------

# combine
dat <- datlist_incidence$ci_age_tabs %>% rbindlist(fill = T, idcol = "sample")

dat[, est := NULL]
dat %>% setnames(old = c("diagnosis", "P", "lower", "upper"), new = c("dg", "est", "lci", "uci"), skip_absent = T)

dat[, value := get_estim(dat, include_unit = F)]

# Women first
dat[, sukup := factor(sukup, levels = c(2, 1), labels =  c("Women", "Men"))]

# maintain order of diagnoses
dat[, dg := factor(dg)]

dat[, dg := tolower(dg)]

#dat[, age := time +1 ]

tbl_cidg <- dat
rm(dat)

## Descriptive table -----------------------------------------------------------

lapply(datlist_incidence$descriptives, function(x) setnames(x, old = "diagnosis", "dg"))

lapply(datlist_incidence$descriptives, function(x) x[, dg := tolower(dg)])

lapply(datlist_incidence$descriptives, 
       function(x) x %<>% 
         .[dgs_tbl, on = "dg", dg_lab := i.labs] %>% 
         .[dg_lab %in% dgs_tbl[subgr == T, labs], dg_lab := paste0("€€", dg_lab)] 
)


## Prevalence age-groups -------------------------------------------------------

dat_preval[ dg %in% c("any", c(paste0("f", seq(1,9)))) & agegr %between% c(5,20) & year == pr_year, sum(`1`, na.rm = T)/ sum(total) *100, keyby = .(sample, sukup, dg)]

# under 35 v f04-f09 excluded
dat_preval[dg == "f04_f09" & agegr<35, `1`:= NA]
dat_preval[dg == "f0" & agegr<35, `1`:= NA]

tbl_preval_agegr_part <- function(agegrs_in = c(0,24), dg_in = c("any", "any_no_f0", dgs_tbl[specific_dgs == 1, dg])){
  t <- dat_preval[ dg %in% dg_in & agegr %between% agegrs_in & year == pr_year & sukup != 0, 
                   .(n = sum(`1`, na.rm = T), tot =sum(total),  pros = sum(`1`, na.rm = T)/ sum(total) *100), 
                   keyby = .(sample, sukup, dg)]
  t[, agegr := paste(agegrs_in, collapse = "–")]
  t[]
}

tbl_preval_agegr <- rbindlist(list(
  tbl_preval_agegr_part(c(0,24)),
  tbl_preval_agegr_part(c(25,49)),
  tbl_preval_agegr_part(c(50,74)),
  tbl_preval_agegr_part(c(75,99)),
  tbl_preval_agegr_part(c(0,99))
))

tbl_preval_agegr[, agegr := factor(agegr, levels = tbl_preval_agegr[, unique(agegr)])]

# FIGURES data -----------------------------------------------------------------

## fig-mainresults: CI-data for figs -------------------------------------------

fig_dat_mr <- list(
  ci = list(all = datlist_incidence$ci_results_all_specialities, #%>% rbindlist(idcol = "dg"), 
            esh = datlist_incidence$ci_results_psychiatry_and_avo_esh, # %>% rbindlist(idcol = "dg"), 
            inpatient = datlist_incidence$ci_results_psychiatry_and_avo_inpat) %>% # rbindlist(idcol = "dg")) %>% 
    rbindlist(idcol = "sample"),
  ir = list(all = datlist_incidence$ir_results_cens_all_specialities  %>% rbindlist(), #idcol = "dg"), 
            esh = datlist_incidence$ir_results_cens_psychiatry_and_avo_esh %>% rbindlist(), #idcol = "dg"), 
            inpatient = datlist_incidence$ir_results_cens_psychiatry_and_avo_inpatient %>% rbindlist()) %>% # idcol = "dg")) %>% 
    rbindlist(idcol = "sample", fill = T),
  pr = dat_preval[sukup != 0])


# hormonize names
fig_dat_mr$ir[,age := age %>% word(sep=fixed("-")) %>% as.numeric()]

fig_dat_mr$ir[, `:=`(lower = lci, upper = uci)]


fig_dat_mr$ci[, x := time +1]
fig_dat_mr$ir[, x := age +1]
fig_dat_mr$pr[, x := agegr]
fig_dat_mr$pr[, est := pros]

lapply(fig_dat_mr, function(x) x[, dg := tolower(dg)])

fig_dat_mr$ir %>% setnames("sex", "sukup")

fig_dat_mr$ir[!dg %in% c("psychoses", "10_no_f0"), dg := paste0("f", dg)]
#fig_dat_mr$ir[dg == "10_no_f0", dg := "any_no_f0"]


fig_dat_mr$pr[dg == "any", dg := "first"]
fig_dat_mr$pr[dg == "any_no_f0", dg := "first_no_f0"]

## save preprocessed data for interactive data visualization -------------------
## f10 == any ci ir -datoissa

estimates_yearly <- list(ci = datlist_incidence$yearly_ci %>% rbindlist(fill = T, idcol = "sample") %>% .[, `:=`(dg = tolower(dg), x = age )],
     ir = fig_dat_mr$ir, 
     pr = fig_dat_mr$pr) %>% rbindlist(idcol = "type", fill = T)

estimates_yearly[, `:=`(lci = lower, uci = upper)][sample == "inpat", sample := "inpatient"]

estimates_yearly %>% fwrite(here("data_processed", "to_interactive.csv"))


## Median aao -----------------------------------------------------------------
# function to calculate median aoo & IQR

# median age of onset 
# by Mai gutvilig
# 2024-06-17

aoo_function <- function(data) {
  
  aoo <- data |>
    as.data.frame() |>
    dplyr::group_by(sukup) |>
    dplyr::mutate(prob = est / 100,
                  pseudo_curve = 1 - prob/max(prob)) |>
    dplyr::mutate(median = dplyr::if_else(pseudo_curve == sign(pseudo_curve - 0.5)*min(abs(pseudo_curve - 0.5)) + 0.5, 1, 0),
                  Q25 = dplyr::if_else(pseudo_curve == sign(pseudo_curve - 0.75)*min(abs(pseudo_curve - 0.75)) + 0.75, 1, 0),
                  Q75 = dplyr::if_else(pseudo_curve == sign(pseudo_curve - 0.25)*min(abs(pseudo_curve - 0.25)) + 0.25, 1, 0))
  
  
  md_w <- mean(aoo[aoo$median == 1 & aoo$sukup == 2,]$time)
  Q25_w <- mean(aoo[aoo$Q25 == 1 & aoo$sukup == 2,]$time)
  Q75_w <- mean(aoo[aoo$Q75 == 1 & aoo$sukup == 2,]$time)
  
  md_m <- mean(aoo[aoo$median == 1 & aoo$sukup == 1,]$time)
  Q25_m <- mean(aoo[aoo$Q25 == 1 & aoo$sukup == 1,]$time)
  Q75_m <- mean(aoo[aoo$Q75 == 1 & aoo$sukup == 1,]$time)
  
  aoo_dt <- data.table(median = c(md_w, md_m),
                       Q25 = c(Q25_w, Q25_m),
                       Q75 = c(Q75_w, Q75_m),
                       sukup = c(2, 1))
  
  # md <- mean(aoo[aoo$median == 1,]$time)
  # Q25 <- mean(aoo[aoo$Q25 == 1,]$time)
  # Q75 <- mean(aoo[aoo$Q75 == 1,]$time)
  # 
  # aoo_dt <- data.table(median = md,
  #                      Q25 = Q25,
  #                      Q75 = Q75)
  # 
  aoo_helper <- aoo_dt[, .(est = median, lci = Q25, uci = Q75, sukup)]
  
  aoo_dt[, value:= get_estim(aoo_helper, include_unit = F, n_decim = 0)]
  
}


median_aoo <- lapply(fig_dat_mr$ci[,unique(sample)], 
                     function(i) lapply(fig_dat_mr$ci[,unique(dg)], 
                                        function(j) fig_dat_mr$ci[dg == j & sample == i] %>% aoo_function() %>% 
                                          .[, `:=`(sample = i, dg = j)]) %>% rbindlist) %>% 
  rbindlist()
median_aoo[sample == "inpatient", sample := "inpat"]
