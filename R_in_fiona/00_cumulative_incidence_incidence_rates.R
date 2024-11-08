# Analysis code for paper
# Lifetime Incidence and Age of Onset of Mental Disorders, and 12–Month Service Utilization

# Variable definitions
# doe: date of entry
# age_doe: age at date of entry
# date: date of event / follow-up end
# age_event: age at event / follow-up end
# status: status at event / follow-up end, 0 = alive, 1 = diagnosis, 2 = death, 3 =  emigration
# sex: 1 = male, 2 = female
# bday: birthdate

# After running this file 
# cumulative incidence (for both sexes) is in dataframe ci
# incidence rate (for men only) is in dataframe crude_ir

#########################

library(survival)
library(tidyverse)
library(data.table)
library(epiR)
library(etm)
library(fst)
library(Epi)
library(popEpi)
library(lubridate)

#########################
# Data

# reading data
data <- read_fst("data.fst")

#########################

# Estimation of cumulative incidence

# estimate cumulatice incidence using Aalen-Johansen estimator
ci_fit <- etmCIF(formula = Surv(age_doe, age_event, status != 0) ~ sex,
                 data = data, etype = status)

# get estimates for sexes and diagnosis event
ci_men <- summary(ci_fit)[[1]][[1]] %>% mutate(sex = 1) #here [[sex=men]][[event=diagnosis]]
ci_women <- summary(ci_fit)[[2]][[1]] %>% mutate(sex = 2) #here [[sex=women]][[event=diagnosis]]

# combine sexes to same df and multiply by 100 to get percentages
ci <- rbind(ci_men, ci_women) %>% 
  mutate(P = P * 100, 
         lower = lower * 100, 
         upper = upper * 100)

# remove unnecessary items
rm(ci_fit, ci_men, ci_women)

#########################

# Estimation of incidence rate

# Function for counting person-years and cases for sexes
pyrs_counts_f <- function(sex) {
  
  # creating the needed variables
  data <- data |>
    mutate(entry_dec = decimal_date(doe), # entry as decimal date
           end_dec = decimal_date(date), # end as decimal date
           bday_dec = decimal_date(bday)) |> # birthday as decimal date
    
    setDT(data)
  
  # one sex at a time
  dats <- data[sex == sex]
  
  # transform to lexis data
  lexis_dat <- Lexis(entry = list(age = entry_dec - bday_dec, 
                                  per = entry_dec),
                     exit = list(per = end_dec),
                     exit.status = factor(status),
                     data = data)
  
  # at this point the following variables are added:
  # age = age at entry, per = decimal date at entry & lex.dur = follow-up time
  
  # then split at every age (1 year splits)
  splits <- round(min(lexis_dat$age), 0):100
  
  split_dat <- splitLexis(lexis_dat, "age", breaks = splits)
  
  setDT(split_dat) # data.table
  
  # remove ages past 100
  split_dat <- split_dat[age < 100]
  
  # creating binary status (1 = sick, 0 = not)
  split_dat <- split_dat[, ':=' (bst = fcase(lex.Xst == "1", 1,lex.Xst != "1", 0),
                                 int_age = floor(age),
                                 int_age_c = floor(age_event))] # seperate int_age for counting cases
  
  # calculate number of person years for each age
  pyrs <- split_dat[, keyby = .(int_age),
                    .(pyrs = sum(lex.dur))] 
  
  # calculate number of cases for each age
  cases <- split_dat[, keyby = .(int_age_c),
                     .(cases = sum(bst))] 
  
  # set similar col names
  setnames(cases, old = "int_age_c", new = "int_age")
  
  # combine pyrs and cases
  pyrs_cases <- pyrs[cases, on = "int_age"]
  
  # keep age less than 100
  pyrs_cases <- pyrs_cases[int_age < 100,] 
  
}

# run the function for sexes
pyrs_counts <- pyrs_counts_f(sex = 1) # example: men

# the ages for which we are estimating incidence ratios
A <- seq(min(pyrs_counts$int_age), 99, 1)
crude_list <- vector("list", length(A))
names(crude_list) <- A

# crude estimates and poisson calculated confidence intervals
for(x in 1:length(A)) {
  crude_model <- glm(cases ~ 1, offset = log(pyrs), family = poisson, data = pyrs_counts[x,])
  result <- as.data.frame(ci.exp(crude_model))
  crude_list[[x]] <- result
}

# add list name to the df 
crude_list <- imap(crude_list, ~.x %>%
                     mutate(age = .y))

# combine into one df
crude_ir <- do.call(rbind, crude_list)
colnames(crude_ir) <- c("est", "lci", "uci", "age") # colnames

# add column for sex
crude_ir <- crude_ir |>
  mutate(sex = 1) # example: men

# per 10,000 pyrs
crude_ir <- crude_ir |>
  mutate(est = est * 10000,
         lci = lci * 10000,
         uci = uci * 10000)

# add columns for cases and pyrs
crude_ir <- cbind(crude_ir, pyrs_counts[,2:3])

# remove unnecessary items
rm(crude_list, crude_model, pyrs_counts, pyrs_counts_f, result, A, x)

####################### Code ends ###########################
