library(fst)
library(data.table)
library(here)
library(flextable)
library(ftExtra)
library(purrr)
library(readxl)
library(magrittr)
library(stringr)
library(ggplot2)
library(ggsci)
library(ggrepel)
library(patchwork)
library(lemon)
library(ggtext)

# Helpers ---------------------------------------------------------------------

# turn numbers into characters with specified format. 
# these well be set to flextables also
numb_in <- list(
  n_decim = 1,
  big_mark = " ",
  decim_mark = ".",
  na_value = "NA"
)
numb <- function(x, n_decim = numb_in$n_decim, big_mark = numb_in$big_mark, decim_mark = numb_in$decim_mark){
  #format(as.numeric(sprintf(paste0("%.", numb_in$n_decim, "f"), x)), big.mark = big_mark) #·
  formatC(x, big.mark = big_mark, decimal.mark = decim_mark, digits = n_decim, format = 'f')
}

# get certain estimates from the results tables
get_estim <- function(dat_in, include_ci = FALSE, estimate_only = FALSE, include_unit = T, ci_lab = "95% CI, ", unit_lab = "%", ci_sep = "–", n_decim = numb_in$n_decim, bracets = c("(", ")")){
  
  if(include_ci == FALSE & include_unit == FALSE){out <- dat_in[, paste0(est %>% numb(), " ", bracets[1], lci %>% numb(), ci_sep, uci %>% numb(), bracets[2])]}
  if(include_ci == TRUE & include_unit == FALSE){out <- dat_in[, paste0(est %>% numb(), " ", bracets[1], ci_lab, lci %>% numb(), ci_sep, uci %>% numb(), bracets[2])]}
  
  if(include_ci == FALSE & include_unit == TRUE){out <- dat_in[, paste0(est %>% numb(), unit_lab, " ", bracets[1], lci %>% numb(), ci_sep, uci %>% numb(), bracets[2])]}
  if(include_ci == TRUE & include_unit == TRUE){out <- dat_in[, paste0(est %>% numb(), unit_lab, " ", bracets[1], ci_lab, lci %>% numb(), ci_sep, uci %>% numb(), bracets[2])]}

  if(estimate_only == TRUE){out <- dat_in[,ifelse(include_unit == T, paste0(est %>% numb(n_decim = n_decim), unit_lab), numb(n_decim = n_decim))]}
  
  out
}

# control the case of the first letter
firstlower <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

firstupper <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# control table size, useful for docx output
fit_table <- function(tbl_in, landscaple = FALSE, margin_mm = 20, page = "A4", supplement = FALSE){
  # A4 210 x 297 mm; 8.3 x 11.7 in
  # A3 297 x 420
  unit_in = "mm"
  page_size <- data.table(page_select = c("A4", "A3"), w = c(210, 297), h = c(297,420))[page_select == page]
  
  if(landscaple == FALSE) {
    tblwidth <-  page_size$w - margin_mm*2 
  } else {
    tblwidth <-  page_size$h - margin_mm*2
  } 
  
  out <- width(tbl_in, width = dim(tbl_in)$widths*tblwidth /(flextable_dim(tbl_in)$widths), unit = unit_in)
  
  if(supplement == FALSE){
    out %<>% fontsize(size = 10, part = "all")
  } else {
    out %<>% 
      fontsize(size = 10, part = "header") %>%
      fontsize(size = 10, part = "body") %>% 
      fontsize(size = 10, part = "footer") %>% 
      font(fontname = "Times", part = "all")
  }
  
  out
}


## GGPLOT theme -----
font_size <- 8
font_fam <- "sans"
theme_set(theme_classic(base_size = font_size)+ theme(
  panel.grid.major.y = element_line(colour = "gray90"),
  text =           element_text(family = font_fam),
  axis.text =      element_text(size = font_size, family = font_fam),
  axis.title =     element_text(size = font_size, family = font_fam),
  plot.title =     element_text(size = font_size, family = font_fam),
  plot.subtitle =  element_text(size = font_size, family = font_fam),
  legend.text =    element_text(size = font_size, family = font_fam)
  ))


# Settings, labels etc ---------------------------------------------------------

#times
tms <- list(
  start = 2000,
  end = 2020,
  inpat = 1975,
  outpat= 1998,
  prim = 2011,
  sens1_start = 2003,
  sens2_start = 2012,
  pr = 2019
)

pr_year <- tms$pr


labs_lst <- list(
  sukup = data.table(val = c(2,1,0), lab = c("Women", "Men", "Total"), ord1= c(2,3,1)),
  sukup_title = "Sex/gener",
  age_title = "Age, y",
  agegr_labs = c("1–4", paste0(seq(5,95,5), "–", seq(9,100,5))),
  agegr_title = "Age-group, y",
  sample = data.table(val = c("all", "esh", "inpatient"), lab = c("All contacts", "Secondary care only", "Inpatient care only")),
  sample_title = "Treatment type",
  ci = "Cumulative incidence",
  ir = "Incidence rate",
  pr = "12-month service utilization",
  ci_unit = "%",
  ir_unit = "per 10 000 PY",
  pr_unit = "%"
)

labs_lst$ci_title <- paste0(labs_lst$ci, ", ", labs_lst$ci_unit)
labs_lst$ir_title <- paste0(labs_lst$ir, " ", labs_lst$ir_unit)
labs_lst$pr_title <- paste0(labs_lst$pr, ", ", labs_lst$pr_unit)


labs_tbls <- list(
  dg = "Diagnosis",
  a25 = "25",
  a50 = "50",
  a75 = "75",
  a100 = "100",
  aoo = "Median AOO (IQR)",
  women = "Women",
  men = "Men",
  ci_title = "Cumulative incidence at given ages (95% CI), %",
  pr_title = "Service utlization, %",
  n_pr_title = "Number of individuals and service utilization by agegroup, N (%)",
  w_to_m = "Gender ratio",
  w_to_m_footnote = "Women to men ratio of the cumulative incidence estimates at the age of 100.",
  n_indiv = "No. of persons at risk at initiation of follow-up",
  n_cases = "No. of new cases during follow-up",
  n_py = "Person-years",
  min_age = "Earliest possible age of onset, y",
  pr_footnote = paste0("Service utilization is the number of individuals with any medical contacts with a diagnosis of a mental disorders during the year ", pr_year, ", divided by the number of individuals in the study population on December 31, ", pr_year, ".")
)

# Diagnoses --------------------------------------------------------------------

f2x <- c("f21", "f22", "f23", "f24", "f25", "f28", "f29")
f2x_labs <- c("Schizotypal disorder (F21)", 
              "Persistent delusional disorders (F22)", 
              "Acute and transient psychotic disorders (F23)", 
              "Induced delusional disorder (F24)", 
              "Schizoaffective disorders (F25)", 
              "Other nonorganic psychotic disorders (F28)", 
              "Unspecified nonorganic psychosis (F29)")

dgs_tbl <- data.table(
  dg = c("any","any_psy", "f_any", "first",
         "any_no_f0", "f_any_no_f0", "first_no_f0",
         "f0", "f00_f03", "f04_f09", 
         "f1", "f10", "f11", "f12", "f13", "f14", "f15", "f16", "f17", "f18", "f19",
         "f2", "f20", "f2_no_sch", 
         f2x,
         "f3", "f30_f31", "f32_f33", "f3_other", 
         "f4", "f40_f41", "f42", "f43", "f44", "f45", "f48", "f4_other", 
         "f5", "f50", "f51", "f53", "f5_other", 
         "f6", "f600", "f601", "f602", "f603", "f604", "f605", "f606", "f607", 
         "f608", "f609", "f61", "f62", "f63_f69",
         "f7", "f70", "f71",  "f72", "f73", "f78", "f79",
         "f8", "f80", "f81", "f82", "f83", "f84", "f88", "f89", 
         "f9", "f90", "f9_other", 
         "f91", "f92", "f93", "f94", "f95", "f98", 
         "f99",
         "psychoses", "imtm"),
  labs = c("Any mental disorder","Any mental disorder", "Any mental disorder", "Any mental disorder",
           "Non-organic mental disorders (F10–F99)", "Non-organic mental disorders (F10–F99)", "Non-organic mental disorders (F10–F99)", #"Any mental disorder, organic mental disorders excluded (F10–F99)",
           "Organic mental disorders (F00–F09)", "Dementias (F00–03)", "Others (F04–09)", 
           "Substance use disorders (F10–F19)",
           "Alcohol use disorders (F10)", "Opiods use disorders (F11)", "Cannabinoids use disorders (F12)", "Sedatives or hypnotics use disorders (F13)",
           "Cocaine use disorders (F14)", "Other stimulants use disorders (F15)", "Hallucinogens use disorders (F16)", "Tobacco use disorders (F17)",
           "Volatile solvents use disorders (F18)", "Multiple and other substances use disorders (F19)",
           "Schizophrenia spectrum (F20–F29)", "Schizophrenia (F20)", "Other (F21–F29)", 
           f2x_labs,
           "Mood disorders (F30–F39)", "Mania and bipolar disorder (F30–F31)", 
           "Depressive disorders (F32–F33)", "Others (F34–F39)", 
           "Neurotic, stress–related and somatoform disorders (F40–F48)", "Anxiety disorders (F40–F41)",
           "Obsessive–compulsive disorder (F42)", "Reaction to severe stress, and adjustment disorders (F43)",
           "Dissociative disorders (F44)", "Somatoform disorders (F45)", "Other neurotic disorders (F48)",
           "Others (F43–F48)", 
           "Behavioral syndromes (F50–F59)", "Eating disorders (F50)", "Nonorganic sleep disorders (F51)", "Disorders associated with the puerperium (F53)", "Other (F54-F59)", 
           "Personality disorders (F60–F69)", 
           "Paranoid personality disorder (F60.0)", "Schizoid personality disorder (F60.1)", "Dissocial personality disorder (F60.2)", 
           "Emotionally unstable personality disorder (F60.3)", "Histrionic personality disorder (F60.4)", "Anankastic personality disorder (F60.5)",
           "Anxious [avoidant] personality disorder (F60.6)", "Dependent personality disorder (F60.7)", "Other specific personality disorders (F60.8)",
           "Personality disorder, unspecified (F60.9)",
           "Mixed and other personality disorders (F61)", "Enduring personality changes, not attributable to brain damage and disease (F62)", "Others (F63–F69)",
           "Intellectual disability (F70–F79)", 
           "Mild intellectual disability (F70)", "Moderate intellectual disability (F71)", "Severe intellectual disability (F72)", "Profound intellectual disability (F73)",
           "Other intellectual disability (F78)", "Unspecified intellectual disability (F79)",
           "Disorders of psychological development (F80–F89)",
           "Specific developmental disorders of speech and language (F80)",
           "Specific developmental disorders of scholastic skills (F81)",
           "Specific developmental disorder of motor function (F82)",
           "Mixed specific developmental disorders (F83)",
           "Pervasive developmental disorders (F84)",
           "Other disorders of psychological development (F88)",
           "Unspecified disorder of psychological development (F89)",
           "Behavioral and emotional disorders (F90–F98)", "ADHD (F90)", "Other (F91–F98)",
           "Conduct disorders (F91)",
           "Mixed disorders of conduct and emotions (F92)",
           "Emotional disorders with onset specific to childhood (F93)",
           "Disorders of social functioning with onset specific to childhood and adolescence (F94)",
           "Tic disorders (F95)",
           "Other behavioral and emotional disorders with onset usually occurring in childhood and adolescence (F98)",
           "Unspecified mental disorder (F99)",
           "All psychoses", "Intentional self-harm (X60–X84)"
  )
)
dgs_tbl[dg == "f9", labs_long := "Behavioral and emotional disorders with onset usually occurring in childhood
and adolescence (F90–F98)"]

dgs_tbl[, subgr := labs %>% str_detect("\\(F\\d\\d")]
dgs_tbl[dg == "f9", subgr := F]

dgs_tbl[, sub_chapter := F][dg %in% paste0("f", seq(0,9)), sub_chapter := T]

dg_min_ages <- list(
  overall = 5,
  dg_specific = 
    list(f7 = 1, f8 = 1, f9= 1, f90 = 1, f9_other = 1, f00_f03 = 35) %>% 
    stack() %>% setDT() %>%  setnames(c('ind', 'values'), c('dg', 'age')) %>% .[,.(dg, age)]
)

dgs_tbl[, min_age := dg_min_ages$overall]
dgs_tbl[dg_min_ages$dg_specific, on = "dg", min_age := i.age]

dgs_tbl[sub_chapter == 1 | dg %in% c("first", "first_no_f0"), labs_short := labs][
  dg == "first", labs_short := "Any mental disorder (F00–F99)"][
    dg == "first_no_f0", labs_short := "Non-organic mental disorders (F10–F99)"][
      dg == "f4", labs_short := "Neurotic disorders (F40–F48)"][
        dg == "f8", labs_short := "Developmental disorders (F80–F89)"][
          dg == "f9", labs_short := "Behavioral disorders (F90–F98)"]

#
dgs_tbl[dg %like% "f7|f8|f9", min_age := 1]
dgs_tbl[dg == "f0", min_age := 35]
dgs_tbl[dg %like% "f04_f09", min_age := 35]

dgs_tbl[, specific_dgs := 0]
dgs_tbl[sub_chapter == T |  grepl("f\\d\\d", dg) | dg %in% c("first", "first_no_f0"), specific_dgs := 1]

dgs_tbl[dg %in% c("f3_other", "f5_other"), specific_dgs := 1]


dgs_tbl[dg %like% "f7|f8|f9"]
