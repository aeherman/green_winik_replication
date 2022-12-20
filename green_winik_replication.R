# read in data
library(tidyverse)
X <- readr::read_csv("data/green_winik_data.csv")
attach(X)
N <- nrow(X)
#### table 1 ####
#recodes variables and tabulates probation level by prison level

probatlevel <-rep(NA,nrow(X)) 
# incjudge: indicator judge to be included in analysis
probatlevel[(incjudge == 1)] <- 0
# probat: probationary period in months
probatlevel[((probat > 0 & probat <= 12 & incjudge == 1))] <- 1
probatlevel[((probat > 12 & probat <= 24 & incjudge == 1))] <- 2
probatlevel[((probat > 24 & probat <= 36 & incjudge == 1))] <- 3
probatlevel[((probat > 36 & incjudge == 1))] <- 4

table(probatlevel)/N

prisonlevel <-rep(NA,nrow(X))
prisonlevel[(incjudge == 1)] <- 0
# toserve: Non-suspended period of incarceration, in months
prisonlevel[((toserve > 0 & toserve <= 12 & incjudge == 1))] <- 1
prisonlevel[((toserve > 12 & toserve <= 24 & incjudge == 1))] <- 2
prisonlevel[((toserve > 24 & toserve <= 36 & incjudge == 1))] <- 3
prisonlevel[((toserve > 36 & incjudge == 1))] <- 4

table(prisonlevel)/N

# Table 1:
t = table(probatlevel,prisonlevel)
t = cbind(t, apply(t, 1, sum))
t = rbind(t, apply(t, 2, sum))
t

#### table 2 ####
#summarizes variables overall, and by calendar
vars_2 <- cbind(age ,agesq ,female ,nonblack ,priorarr ,priordrugarr ,priorfelarr ,priorfeldrugarr ,priorcon ,priordrugcon ,priorfelcon ,priorfeldrugcon ,pwid ,dist ,marijuana ,cocaine ,crack ,heroin ,pcp ,otherdrug ,nondrug)
apply(vars_2, 2, sd)
apply(vars_2, 2, summary)[c(1,4,6),]
X %>% select(calendar, colnames(vars_2)) %>%
    group_by(calendar) %>% summarize(across(colnames(vars_2), ~signif(mean(.), 3))) %>% t %>%
    janitor::row_to_names(1, remove_rows_above = FALSE)

#### table 3 ####
# table 3 #/  ## summarizes more variables by calendar
vars_3 <- c("laterarr", "incarcerate", "toserve", "probat", "probatnonzero")
X %>% select(calendar, all_of(vars_3)) %>%
    group_by(calendar) %>% summarize(across(vars_3, ~signif(mean(.), 3))) %>% t %>%
    janitor::row_to_names(1, remove_rows_above = FALSE)

#### table 4 ####
## regressions and tests of linear hypotheses of outcomes on covariates with some different empirical specifications (all Ordinary Least Squares)

regout <- lm( incarcerate ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 , data=X, subset=(incjudge == 1))
summary(regout)
regout <- lm( incarcerate ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
summary(regout)

regout <- lm( toserve ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 , data=X, subset=(incjudge == 1))
summary(regout)
regout <- lm( toserve ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
summary(regout)

regout <- lm( probatnonzero ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 , data=X, subset=(incjudge == 1))
summary(regout)
regout <- lm( probatnonzero ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
summary(regout)

regout <- lm( probat ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 , data=X, subset=(incjudge == 1))
summary(regout)
regout <- lm( probat ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
summary(regout)

# table 5 #/  ## instrumental variables estimation -- vector of calendar dummies are instruments; toserve and sometimes probat are endogenous explanatory variables, depending on specifications; robust standard errors are clustered on the clusterid var.  
## error: unexpected symbol in "ivreg2 laterarr"
ivreg2 laterarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterarr (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

library(AER)
library(MASS)
# The "ivreg" function in the package "AER" does two-stage least squares (2SLS).  The same can be done using
# the function "tsls" in the package "sem".
ivout1 <- ivreg(laterarr ~ toserve | as.factor(calendar))
summary(ivout1)
ivout2 <- ivreg(laterarr ~ age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug + probat | as.factor(calendar))
summary(ivout2)
ivout2
# The model fits, but the summary() command fails.  Only the first 9 variables get a fitted coefficient, the rest recieve NA's.
## warning: more regressors than instruments

ivout3 <- ivreg(laterarr ~ probat | as.factor(calendar))
summary(ivout3)
ivout4 <- ivreg(laterarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug | as.factor(calendar))
summary(ivout4)  # Doesn't work yet.
## warning: more regressors than instruments
ivout5 <- ivreg(laterarr ~ toserve + probat | as.factor(calendar))
summary(ivout5)
ivout6 <- ivreg(laterarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug | as.factor(calendar))
summary(ivout6)  # Doesn't work yet.
## warning: more regressors than instruments


# table 6 #/  ## OLS regressions, with robust standard errors clustered on clusterid

regout <- rlm( laterarr ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( laterarr ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterarr ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( laterarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterarr ~ toserve + probat , data=X, subset=(incjudge == 1))
regout <- rlm( laterarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))

# table 7 #/  ## instrumental variables estimation, estimated using limited information maximum likelihood, with robust standard errors clustered on clusterid var.
## look into ivmodel package
ivregress liml laterarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivregress liml laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivregress liml laterarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivregress liml laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivregress liml laterarr (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivregress liml laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
# have been unable to find an instrumental-variables package in R that implements the LIML algorithm.


# Suspended Sentence Robustness Check #/    ##estimates OLS and IV regressions of laterarr with clustered standard errors
##note that ivregress and ivreg2 are two different Stata commands that are both used for instrumental variables estimation.  ivregress is the Stata-supported command and ivreg2 is a user written command that sometimes reports more properties of the instrument, depending on which options are included with ivregress.  

ivreg2 laterarr (toserve suspend probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve suspend probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterarr (toserve suspend probat probsuspend = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve suspend probat probsuspend = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

regout <- rlm( laterarr ~ toserve + suspend + probat , data=X, subset=(incjudge == 1))
regout <- rlm( laterarr ~ toserve + suspend + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterarr ~ toserve + suspend + probat + probsuspend , data=X, subset=(incjudge == 1))
regout <- rlm( laterarr ~ toserve + suspend + probat + probsuspend + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))

# Simulation of time on the street #/

# generate censoring indicator #/
fail<-rep(NA,nrow(X))
fail <- 0
fail[(fullreleasetorecid != NA)] <- 1
## error: fullreleasetorecid not found

# create a survival time variable that topcodes missing data #/
failtime<-rep(NA,nrow(X))
failtime <- lullreleasetorecid
failtime[(failtime= = NA)] <- 1600
## error: lullreleasetorecid not found

# define survival data with topcode=1600 #/
stset failtime, failure(fail)

# predicts cumulative survival given _t = failtime #/
###  streg performs maximum likelihood estimation for parametric regression
   # survival-time models.  streg can be used with single- or multiple-record
   # or single- or multiple-failure st data.  Survival models currently
   # supported are exponential, Weibull, Gompertz, lognormal, loglogistic, and
   # generalized gamma.  Parametric frailty models and shared-frailty models
   # are also fit using streg.  ###/



regout <- rlm( age ~ agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1 distribution(weibull)))
predict cs, csurv
rename cs cs1

# replaces _t with 1461 instead of failtime #/

_t[(NA NA 1461)] <- 1461
## predicts csm simulation and creates new variable called csurv

predict cs, csurv

# runs new regressions #/
## generates new variables
laterarrsim<-rep(NA,nrow(X))
laterarrsim <- 0
beta<-rep(NA,nrow(X))
beta <- 0
stderror<-rep(NA,nrow(X))
stderror <- 0
counter<-rep(NA,nrow(X))
counter <- 0

## for values is a loop, such that each of the statements included within the loop (replace, ivregress, etc. ) are repeated for each i from 1 to 1000,
## this loop recodes the laterarrsim variable and then performs instrumental variables estimation using limited information maximum likelihood,
## in the last line of theloop, the coefficients (betas) and corresponding errors from the instrumental variables procedure are saved to the beta and standard error variables,


forvalues i = 1(1)1000 {
counter[(1)] <- counter
laterarrsim[(1 - cs))] <- floor(uniform()
laterarrsim[(laterarr == 1)] <- 1
laterarrsim[(laterarr == 0 & incarcerate == 0)] <- 0
quietly ivregress liml laterarrsim age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
beta[(_n= = counter)] <- _b[toserve]
stderror[(_n= = counter)] <- _se[toserve]
}

## this statement, sum, is short for summarize, which here summarizes the beta and standard errors from the loop above

summary(beta ,stderror ,if ,_n ,<= ,counter)

# Robustness Assessments #/

#These assessments fall into five major categories:

#A: Robustness using only a subset of the strongest instruments (first stage F > 10)
#B: Robustness to various metrics of recidivism
#C: Robustness to partition by prior conviction
#D: Robustness to exclusion of non-convicted defendants

#Table of Contents

#A1: Find Strongest Subsets of Instruments for toserve and Show Robustness of Results
#A2: Find Strongest Subsets of Instruments for probat and Show Robustness of Results
#A3: Examine Effects of Incarceration and Probation of Any Length

#B1: Later Drug Arrest as Recidivism Metric
#B2: Later Felony Arrest as Recidivism Metric
#B3: Later Felony Drug Arrest as Recidivism Metric
#B4: Later Non-Felony Arrest as Recidivism Metric
#B5: Later Conviction as Recidivism Metric
#B6: Later Drug Conviction as Recidivism Metric
#B7: Later Felony Conviction as Recidivism Metric
#B8: Later Non-Felony Conviction as Recidivism Metric
#B9: Later Felony Drug Conviction as Recidivism Metric

#C1: Partition--Defendants Who Have a Prior Conviction
#C2: Partition--Defendants Who Have No Prior Conviction

#D: Partition--Only Convicted Defendants

#E: Simulate Effects of Specific Deterrence After Eliminating Effects of Incapacitation

#Code

#A1: Find Strongest Subsets of Instruments for toserve and Show Robustness of Results
## these are OLS and IV estimates with clustered standard errors where calendar or a series of calendar dummies depending on the specification, instrument for the variable toserve

regout <- lm( toserve ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 , data=X)
ivreg2 laterarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) , robust cluster(clusterid) first
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9), robust cluster(clusterid) first

regout <- lm( toserve ~ calendar2 + calendar3 + calendar4 + calendar9 , data=X)
ivreg2 laterarr (toserve = calendar2 calendar3 calendar4 calendar9) , robust cluster(clusterid) first
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar2 calendar3 calendar4 calendar9), robust cluster(clusterid) first
     
regout <- lm( toserve ~ calendar3 + calendar4 , data=X)
ivreg2 laterarr (toserve = calendar3 calendar4), robust cluster(clusterid) first
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar3 calendar4), robust cluster(clusterid) first

regout <- lm( toserve ~ calendar3 , data=X)
ivreg2 laterarr (toserve = calendar3) , robust cluster(clusterid) first
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar3), robust cluster(clusterid) first

#A2: Find Strongest Subsets of Instruments for probat and Show Robustness of Results

regout <- lm( probat ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar7 + calendar8 + calendar9 , data=X)
ivreg2 laterarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9), robust cluster(clusterid) first
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9), robust cluster(clusterid) first

regout <- lm( probat ~ calendar4 + calendar7 + calendar8 + calendar9 , data=X)
ivreg2 laterarr (probat = calendar4 calendar7 calendar8 calendar9), robust cluster(clusterid) first
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar4 calendar7 calendar8 calendar9), robust cluster(clusterid) first

#A3: Examine Effects of Incarceration and Probation of Any Length

## These are more IV estimates where there are multiple endogenous regressions and multiple instruments
## Models are estimated using both two-stage least squares (2sls in the syntax) and limited information maximum likelihood (liml in the syntax)

ivregress 2sls laterarr (incarcerate probatnonzero = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9), robust cluster(clusterid) 
ivregress 2sls laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (incarcerate probatnonzero = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9), robust cluster(clusterid) 

ivregress liml laterarr (incarcerate probatnonzero = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9), robust cluster(clusterid) 
ivregress liml laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (incarcerate probatnonzero = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9), robust cluster(clusterid) 

#B1: Later Drug Arrest as Recidivism Metric

regout <- rlm( laterdrugarr ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugarr ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugarr ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugarr ~ toserve + probat , data=X, subset=(incjudge == 1))

ivreg2 laterdrugarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugarr (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

#B2: Later Felony Arrest as Recidivism Metric
## More robustness checks -- reg is always OLS and ivregress is 2sls or liml depending on options, and ivreg2 is 2sls


regout <- rlm( laterfelarr ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelarr ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelarr ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelarr ~ toserve + probat , data=X, subset=(incjudge == 1))

ivreg2 laterfelarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelarr (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

#B3: Later Felony Drug Arrest as Recidivism Metric

regout <- rlm( laterfeldrugarr ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugarr ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugarr ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugarr ~ toserve + probat , data=X, subset=(incjudge == 1))

ivreg2 laterfeldrugarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugarr (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

#B4: Later Non-Felony Arrest as Recidivism Metric

laternonfelarr <-rep(NA,nrow(X))
laternonfelarr <- laterarr

regout <- rlm( laternonfelarr ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelarr ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelarr ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelarr ~ toserve + probat , data=X, subset=(incjudge == 1))

ivreg2 laternonfelarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelarr (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

#B5: Later Conviction as Recidivism Metric

regout <- rlm( latercon ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( latercon ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( latercon ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( latercon ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( latercon ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( latercon ~ toserve + probat , data=X, subset=(incjudge == 1))

ivreg2 latercon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 latercon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 latercon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 latercon (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 latercon (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 latercon (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

#B6: Later Drug Conviction as Recidivism Metric

regout <- rlm( laterdrugcon ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugcon ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugcon ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugcon ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugcon ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( laterdrugcon ~ toserve + probat , data=X, subset=(incjudge == 1))

ivreg2 laterdrugcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugcon (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugcon (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterdrugcon (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

#B7: Later Felony Conviction as Recidivism Metric

regout <- rlm( laterfelcon ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelcon ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelcon ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelcon ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelcon ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( laterfelcon ~ toserve + probat , data=X, subset=(incjudge == 1))

ivreg2 laterfelcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelcon (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelcon (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfelcon (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

#B8: Later Non-Felony Conviction as Recidivism Metric

laternonfelcon<-rep(NA,nrow(X))
laternonfelcon <- latercon

regout <- rlm( laternonfelcon ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelcon ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelcon ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelcon ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelcon ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( laternonfelcon ~ toserve + probat , data=X, subset=(incjudge == 1))

ivreg2 laternonfelcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelcon (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelcon (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laternonfelcon (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

#B9: Later Felony Drug Conviction as Recidivism Metric

regout <- rlm( laterfeldrugcon ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugcon ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugcon ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugcon ~ toserve , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugcon ~ probat , data=X, subset=(incjudge == 1))
regout <- rlm( laterfeldrugcon ~ toserve + probat , data=X, subset=(incjudge == 1))

ivreg2 laterfeldrugcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugcon (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugcon (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)
ivreg2 laterfeldrugcon (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) level(90)

#C1: Partition--Defendants Who Have a Prior Conviction

regout <- rlm( laterarr ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=((incjudge == 1 & priorcon == 1)))
regout <- rlm( laterarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=((incjudge == 1 & priorcon == 1)))
regout <- rlm( laterarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=((incjudge == 1 & priorcon == 1)))
regout <- rlm( laterarr ~ toserve , data=X, subset=((incjudge == 1 & priorcon == 1)))
regout <- rlm( laterarr ~ probat , data=X, subset=((incjudge == 1 & priorcon == 1)))
regout <- rlm( laterarr ~ toserve + probat , data=X, subset=((incjudge == 1 & priorcon == 1)))

ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 1), robust cluster(clusterid) level(90)

#C2: Partition--Defendants Who Have No Prior Conviction

regout <- rlm( laterarr ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=((incjudge == 1 & priorcon == 0)))
regout <- rlm( laterarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=((incjudge == 1 & priorcon == 0)))
regout <- rlm( laterarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=((incjudge == 1 & priorcon == 0)))
regout <- rlm( laterarr ~ toserve , data=X, subset=((incjudge == 1 & priorcon == 0)))
regout <- rlm( laterarr ~ probat , data=X, subset=((incjudge == 1 & priorcon == 0)))
regout <- rlm( laterarr ~ toserve + probat , data=X, subset=((incjudge == 1 & priorcon == 0)))

ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 0), robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 0), robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 0), robust cluster(clusterid) level(90)
ivreg2 laterarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 0), robust cluster(clusterid) level(90)
ivreg2 laterarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 0), robust cluster(clusterid) level(90)
ivreg2 laterarr (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & priorcon == 0), robust cluster(clusterid) level(90)

#D: Partition--Only Convicted Defendants

regout <- rlm( laterarr ~ toserve + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=((incjudge == 1 & conviction == 1)))
regout <- rlm( laterarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=((incjudge == 1 & conviction == 1)))
regout <- rlm( laterarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=((incjudge == 1 & conviction == 1)))
regout <- rlm( laterarr ~ toserve , data=X, subset=((incjudge == 1 & conviction == 1)))
regout <- rlm( laterarr ~ probat , data=X, subset=((incjudge == 1 & conviction == 1)))
regout <- rlm( laterarr ~ toserve + probat , data=X, subset=((incjudge == 1 & conviction == 1)))

ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & conviction == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & conviction == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & conviction == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & conviction == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & conviction == 1), robust cluster(clusterid) level(90)
ivreg2 laterarr (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if (incjudge == 1 & conviction == 1), robust cluster(clusterid) level(90)

#E: Simulate Effects of Specific Deterrence After Eliminating Effects of Incapacitation

#The idea is to generate simulated outcome data (correcting for incapacitation) and then repeat
#earlier 2SLS and LIML analyses using simulated outcomes

set more off

# generate censoring indicator #/
fail <-rep(NA,nrow(X))
fail <- 0
fail[(fullreleasetorecid ! = NA)] <- 1

# create a survival time variable that topcodes missing data #/
# note that it also codes failtime=1 if failtime==0 so as to avoid dropping cases when running streg below #/
failtime<-rep(NA,nrow(X))
failtime <- lullreleasetorecid
failtime[(failtime= = NA)] <- 1600
failtime[(failtime= = 0.)] <- 1

# define survival data with topcode=1600 #/
## note that the stset code declares the Stata dataset to be survival time data.  this is a necessary step in Stata before using the streg command two lines down
stset failtime, failure(fail)

# predicts cumulative survival given _t = failtime #/
regout <- rlm( age ~ agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1 distribution(weibull)))
predict cs, csurv
rename cs cs1

### As noted above, streg performs maximum likelihood estimation for parametric regression
    #survival-time models.  streg can be used with single- or multiple-record
    #or single- or multiple-failure st data.  Survival models currently
    #supported are exponential, Weibull, Gompertz, lognormal, loglogistic, and
    #generalized gamma.  Parametric frailty models and shared-frailty models
    #are also fit using streg.  ###/
## to see more information about this or other specific stata commands, type "help" followed by the name of the command in the Stata window.





# replaces _t with 1461 instead of failtime #/
_t[(NA NA 1461)] <- 1461
predict cs, csurv

# runs new regressions #/
# note that this one can use either LIML or 2SLS, so check the defaults for ivreg2 #/
## the default for ivreg2 is 2SLS

laterarrsim<-rep(NA,nrow(X))
laterarrsim <- 0
beta<-rep(NA,nrow(X))
beta <- 0
stderror<-rep(NA,nrow(X))
stderror <- 0
beta2<-rep(NA,nrow(X))
beta2 <- 0
stderror2<-rep(NA,nrow(X))
stderror2 <- 0

counter<-rep(NA,nrow(X))
counter <- 0

## forvalues is a looop, in this case looping the coding of variables (replace commands) and instrumental variables procedures (ivreg2)


forvalues i = 1(1)1000 {
counter[(1)] <- counter
laterarrsim[(1 - cs))] <- floor(uniform()
laterarrsim[(laterarr == 1)] <- 1
laterarrsim[(laterarr == 0 & incarcerate == 0)] <- 0

quietly ivreg2 laterarrsim age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) liml
beta[(_n= = counter)] <- _b[toserve]
stderror[(_n= = counter)] <- _se[toserve]

quietly ivreg2 laterarrsim age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) 
beta2[(_n= = counter)] <- _b[toserve]
stderror2[(_n= = counter)] <- _se[toserve]

}
# Print out results: LIML(beta) LIML(SE) 2SLS(beta) 2SLS(SE) #/
summary(beta ,stderror ,beta2 ,stderror2 ,if ,_n ,<= ,counter)

## sum is short for summarize which summarizes descriptive statistics, in this case the point estimate and standard error for the limited information maximum likelihood (LIML) and two-stage least squares (2SLS) estimation procedures


# uncomment lines below in order to get percentiles #/
## generating t and t2 as variables

t<-rep(NA,nrow(X))
t <- leta/stderror
t2<-rep(NA,nrow(X))
t2 <- leta2/stderror2


## tabulate variables
# tab t
# tab t2

# Hausman test comparing instrumental variables and OLS estimates of specific deterrence effects 
# note that including agesq causes Stata to give a warning, but excluding agesq has no effect on the test
# 

ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) 
est store instrumental_variables
regout <- rlm( laterarr ~ age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug + toserve , data=X, subset=(incjudge == 1))
est store OLS
### these four lines of code estimate 2SLS instrumental variables models, store the estimates as  "instrumental_variables" 
### then estimates the same model via OLS, stores those estimates.   The Huasman test then compares the two sets of estimates
### can find more information about the Hausman test in any econometrics book or by typing help hausman into Stata.




# Note from the Stata Manual:
#     "The order of computing the two estimators may be reversed. You have to be careful, though, to specify to hausman the
#     models in the order "always consistent" first and "efficient under H0" second."


### this is the Stata command to run the Huasman test to compare the two sets of previously stored estimates.  the force option is used to compute the test even though some of its theoretical assumptions may not be met given the data
hausman instrumental_variables OLS,force

# results
#  (b-B)     sqrt(diag(V_b-V_B))
# .0142088        .0081353

# chi2(1) = (b-B)'[(V_b-V_B)^(-1)](b-B)
#         = .0142088^2 / .0081353^2 = 3.05

# repeat test, with probation instead of toserve'
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) 
est store instrumental_variables
regout <- rlm( laterarr ~ probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
est store OLS

hausman instrumental_variables OLS,force


# repeat test, with probation and toserve

ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug (toserve probat = calendar1 calendar2 calendar3 calendar4 calendar5 calendar6 calendar7 calendar8 calendar9) if incjudge == 1, robust cluster(clusterid) 
est store instrumental_variables
regout <- rlm( laterarr ~ toserve + probat + age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug , data=X, subset=(incjudge == 1))
est store OLS

hausman instrumental_variables OLS,force

# Tests of significance based on reduced form regression of later arrest on calendar assignment
# The tests are done three ways: regression with clustering, regression without clustering, and random effects regression
set more off

regout <- rlm( laterarr ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar6 + calendar7 + calendar8 + calendar9 , data=X, subset=(incjudge == 1))
summary(regout)

regout <- rlm( laterarr ~ age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug + calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar6 + calendar7 + calendar8 + calendar9 , data=X, subset=(incjudge == 1))
summary(regout)


regout <- lm( laterarr ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar6 + calendar7 + calendar8 + calendar9 , data=X, subset=(incjudge == 1))
summary(regout)

regout <- lm( laterarr ~ age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug + calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar6 + calendar7 + calendar8 + calendar9 , data=X, subset=(incjudge == 1))
summary(regout)

### xtreg is a random effects regression where the re option declares random effects.   
### iis is outdated and has been replaced by the xtset command. 
### must declare the unit for the random effects before running an xtreg command with the re option
### in this case the synatx in Stata is xtset clusterid (rather then the outdated iis clusterid command)
iis clusterid
regout <- lm( laterarr ~ calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar6 + calendar7 + calendar8 + calendar9, + re , data=X)
summary(regout)

regout <- lm( laterarr ~ age + agesq + female + nonblack + priorarr + priordrugarr + priorfelarr + priorfeldrugarr + priorcon + priordrugcon + priorfelcon + priorfeldrugcon + pwid + dist + marijuana + cocaine + crack + heroin + pcp + otherdrug + nondrug + calendar1 + calendar2 + calendar3 + calendar4 + calendar5 + calendar6 + calendar7 + calendar8 + calendar9, + re , data=X)
summary(regout)
