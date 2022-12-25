library(tidyverse)
library(AER)
library(ivmodel)
library(plm) # OLS iv regression
source("functions/liml.R")

X <- readr::read_csv("data/green_winik_data.csv") %>%
  mutate(across(ends_with("date"), ~as.Date(., "%m/%d/%Y"), .names = "{col}_"))
attach(X)
N <- nrow(X)

exogenous <- cbind(age, agesq, female, nonblack, priorarr, priordrugarr, priorfelarr, priorfeldrugarr,
                   priorcon, priordrugcon, priorfelcon, priorfeldrugcon, pwid, dist,
                   marijuana, cocaine, crack, heroin, pcp, otherdrug, nondrug)

theme_set(ggthemes::theme_tufte(ticks = F))
