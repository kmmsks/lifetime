library(data.table)
library(here)
library(magrittr)
library(stringr)

year <- 2009
agegr_years <- 1

dgs_of_interest <- # a data.table with columns 
  # dg: working name of the group; 
  #dg_code: icd-10 codes in the group, a string with codes separeted by "|",
  # example: 
  # dg , dg_code
  # "f0" , "f0",
  # "f00_f03", "f00|f01|f02|f03"


# Healthcare data
hilmo <- # a. data.table with data on all relevant healthcare contacts during `year` 

d0 <- # # a. data.table with data on the total population at the end of the `year`

d0 <-  d0 %>% unique(by = "shnro") # one row per person, there may be few duplicated in the population data

d0 <- d0[!is.na(shnro)]

n <- paste0("agegr_", agegr_years)
d0[, (n):= cut(ika, breaks = seq(0, max(ika) + agegr_years, agegr_years), right = FALSE, include.lowest = TRUE) %>% 
     str_sub(start = 2) %>% str_extract("[^,]+") %>% as.numeric()]


hilmo_unique <- hilmo[vuosi == year, .(inpat = any(overnight = TRUE), dg_psy := paste0(dg_psy, collapse = "_"), dg_avo := paste0(dg_avo, collapse = "_")), shnro]

hilmo_unique[, dg := paste(dg_psy, dg_avo, sep = ":")]
hilmo_unique[, esh := 0][dg_avo == "NA", esh := 1]

# Helpers

create_by_sample <- function(dat_in){
  dat <- dat_in
  fun <- function(x){
    dat[grepl(dgs_of_interests[dg == x, dg_code], dg, ignore.case = T), paste(x) := 1]
  }
  lapply(dgs_of_interests, fun)
  dat <- dat[, -c("dg", "dg_psy", "dg_avo")]
  
  dat
}

count_su_by_sukup <- function(dat_in, dg_in){
  a <- paste0("agegr_", agegr_years)
  dat <- dat_in[, .(n = .N), keyby = c(eval(dg_in), eval(a), "sukup")]
  setnames(dat, old = c(a, dg_in), new = c("agegr", "dg"))
  
  d <- dat %>% dcast(agegr+sukup~dgm value.var = "n")
  
  if(!("1" %in% colnames(d))){d[, `1`:= 0]}
  
  d[, total := `0` + `1`]
  d[, pros := `1` / total * 100, by = sukup]
  d
}


hilmo_dat <- list(
  all = create_by_sample(hilmo_unique),
  esh = create_by_sample(hilmo_unique[esh == 1]),
  inpatient = create_by_sample(hilmo_unique[inpat == 1])
)


s <- hilmo_dat %>% names()
names(s) <- s

# The population and info on each diagnosis whether present or not during the year, all, seocdanry care and inpatient care separately 
d1 <- lapply(s,
             function(x) 
               merge(
                 d0,
                 hilmo_dat[[x]],
                 by = "shnro",
                 all.x = T
               )
             )

# Fill dg_columns na -> 0
lapply(d1, function(x) setnafill(x, fill = 0, cols = c(dg_groups _w_min_ages$dg %>% as.character())))

# F0 based on the earliest possible age
lapply(d1, function(x) x[, fo := 0][ika < 35, f_00_f03 := 0][f00_f03 ==1 | f04_f09 == 1, f0 := 1])

# any f diagnosis
cols <- dg_maingroups %>% unlist()

lapply(d1, function(x) x[, any := 0])
lapply(d1, function(x) x[x[, Reduce(`|`, lapply(.SD, `==`, 1)), .SDcols = cols], any := 1])

# any F except f0
cols <- cols %>% setdiff("f0")
lapply(d1, function(x) x[, any_no_f0 := 0])
lapply(d1, function(x) x[x[, Reduce(`|`, lapply(.SD, `==`, 1)), .SDcols = cols], any_no_f0 := 1])

# 
dgs <- c("any", "any_no_f0", dgs_of_interests$dg %>% as.character())

service_utlization_by_sukup <- lapply(d1, 
                                      function(i)
                                        lapply(function(j)
                                          count_su_by_sukup(dat_in = i, dg_in = j)
                                          ) %>% rbindlist(idcol = "dg")
                                      ) %>% rbindlist(idcol = "sample")


out <- service_utlization_by_sukup[agegr < 101]

out[is.na(`1`), total := `0`]
out[`1` <= 3 | is.na(`1`), `:=`(`0` = NA, `1` = NA, pros = NA)]

out %>% fwrite(here("service_utlization.csv"))
