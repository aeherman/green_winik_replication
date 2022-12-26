library(tidyverse)
library(AER)
library(ivmodel)
library(plm) # OLS iv regression
library(tseries)
source("functions/liml.R")

X <- readr::read_csv("data/green_winik_data.csv") %>%
  mutate(across(ends_with("date"), ~as.Date(., "%m/%d/%Y"), .names = "{col}_"))

attach(X)
N <- nrow(X)

exogenous <- cbind(age, agesq, female, nonblack, priorarr, priordrugarr, priorfelarr, priorfeldrugarr,
                   priorcon, priordrugcon, priorfelcon, priorfeldrugcon, pwid, dist,
                   marijuana, cocaine, crack, heroin, pcp, otherdrug, nondrug)

endogenous <- c("probat", "toserve", "incarcerate", "probatnonzero", "incarc")

# harshness as:
# 1. toserve
# 2. toserve/incaceration, if incarceration were predetermined, but not suspension, but bad if probation is less likely

rank_harshness <- function(m) {
  sym_m <- sym(m)
  X %>% group_by(calendar) %>% summarize(across(c("probat", "toserve"), ~mean(.))) %>%
    arrange(!!sym_m)
}

rank_toserve <- match(calendar, rank_harshness("toserve")$calendar)
rank_probat <- match(calendar, rank_harshness("probat")$calendar)

theme_set(ggthemes::theme_tufte(ticks = F))
