library(tidyverse)
library(AER)
library(ivmodel)
source("liml.R")

X <- readr::read_csv("data/green_winik_data.csv") %>%
  mutate(across(ends_with("date"), ~as.Date(., "%m/%d/%Y"), .names = "{col}_"))
attach(X)
N <- nrow(X)

exogenous <- cbind(age, agesq, female, nonblack, priorarr, priordrugarr, priorfelarr, priorfeldrugarr,
                   priorcon, priordrugcon, priorfelcon, priorfeldrugcon, pwid, dist,
                   marijuana, cocaine, crack, heroin, pcp, otherdrug, nondrug)
