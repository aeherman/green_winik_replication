# read in data
source("start_up.R")
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
table1 = table(probatlevel,prisonlevel)
table1 = cbind(t, apply(t, 1, sum))
table1 = rbind(t, apply(t, 2, sum))
table1

#### table 2 ####
#summarizes exogenous variables overall, and by calendar
exogenous <- cbind(age ,agesq ,female ,nonblack ,priorarr ,priordrugarr ,priorfelarr ,priorfeldrugarr ,priorcon ,priordrugcon ,priorfelcon ,priorfeldrugcon ,pwid ,dist ,marijuana ,cocaine ,crack ,heroin ,pcp ,otherdrug ,nondrug)
apply(exogenous, 2, sd)
apply(exogenous, 2, summary)[c(1,4,6),]
X %>% select(calendar, colnames(exogenous)) %>%
    group_by(calendar) %>% summarize(across(colnames(exogenous), ~signif(mean(.), 3))) %>% t %>%
    janitor::row_to_names(1, remove_rows_above = FALSE)

#with(X, do.call(rbind, tapply(age, calendar, function(x) c(M = mean(x), SD = sd(x)))))
# tmp is the distribution for the null hypothesis: https://stackoverflow.com/questions/36763010/retrieving-the-monte-carlo-simulation-values-for-chi-square-test
# missing the p-value for the reg
table_2_reg <- lapply(setNames(colnames(exogenous), colnames(exogenous)), function(var) {
    reg <- nnet::multinom(formula(glue::glue("calendar ~ {var}")), data = X)
    chi <- chisq.test(X %>% select(calendar, all_of(var)), simulate.p.value = T, B=1000)$p.value
    return(list(reg = reg, chi = chi))
})

lapply(table_2_reg, function(var) var$chi)


#chisq.test(X %>% select(agesq), simulate.p.value = T, B = 1000)$p.value
# age, agesq, nonblack, check dist. later

#### table 3 ####
# table 3 #/  ## summarizes endogenous variables by calendar
endogenous <- c("toserve", "incarc", "incarcerate", "toserve", "probat", "probatnonzero")
table3 <- X %>% select(calendar, all_of(endogenous)) %>%
    group_by(calendar) %>% summarize(across(all_of(endogenous), ~signif(mean(.), 3))) %>% t %>%
    janitor::row_to_names(1, remove_rows_above = FALSE)

table3 <- bind_cols(endogenous = rownames(table3), as_tibble(table3))
table3
#with(X, do.call(cbind, tapply(toserve, calendar, function(x) c(M = mean(x), SD = sd(x)))))
#group_by(X, calendar) %>% summarize(M = mean(toserve), SD = sd(toserve))

en_vars <- c("toserve", "probat", "incarc")
X %>%
  group_by(calendar) %>%
  mutate(across(all_of(en_vars), mean, .names = "{col}_mean")) %>%
  select(calendar, all_of(en_vars), ends_with("_mean")) %>%
  pivot_longer(all_of(en_vars), names_to = "sentence", values_to = "length") %>%
  ggplot() +
  ggridges::geom_density_ridges(
    aes(x = length, y = reorder(as.factor(calendar), toserve_mean), fill = sentence),
    scale = 1.2, alpha = 0.50, quantile_lines = T, quantile_fun = median) +
  xlim(0, NA) +
  facet_grid(. ~ sentence, scales = "free_x", space = "free_x") +
  scale_x_continuous(n.breaks = 4, limits = c(0, NA)) +
  theme(legend.position = "none") +
  ylab("Calendar\n") + xlab("\nLength") + ggtitle("Distribution of Continuous Treatment Variables") +
  geom_vline(aes(xintercept = 50))


  
#### table 4 ####
## regressions and tests of linear hypotheses of outcomes on covariates with some different empirical specifications (all Ordinary Least Squares)
covariates <- paste(colnames(exogenous), collapse = ' + ')
calendars <- paste0('calendar', 1:9, collapse = ' + ')

ftest <- function(x, cov = NULL, d = subset(X, incjudge == 1)) {
  form <- glue::glue("{x} ~ {calendars}")
  hascov <- "no"
  
  if(!is.null(cov)) {
    form <- glue::glue("{form} + {cov}")
    hascov <- "yes"
  }
  s <- summary(lm(formula(form), data = d))
  f <- s$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = F)
  
  return(list(output = c(depvar = x, hascov = hascov, signif(f, 4), pvalue = signif(p, 4)),
              summary = s))
}

table4 <- bind_rows(lapply(c("incarcerate", "toserve", "probatnonzero", "probat"), function(x) ftest(x)$output),
          lapply(c("incarcerate", "toserve", "probatnonzero", "probat"), function(x) ftest(x, cov = covariates)$output))
table4

ftest("toserve")$summary
summary(lm(formula(glue::glue("toserve ~ {calendars}")), X))$coef %>% as_tibble %>% mutate(calendar = 1:9) %>%
  arrange(Estimate)
# weak instrument

# table 5 #/  ## instrumental variables estimation -- vector of calendar dummies are instruments; toserve and sometimes probat are endogenous explanatory variables, depending on specifications; robust standard errors are clustered on the clusterid var.  
## error: unexpected symbol in "ivreg2 laterarr"

calendars <- paste0('calendar', 1:9, collapse = ' + ')
# calendars are confounded with length of sentence, which can't be randomly assigned, but calendar only affects y through x
# adding the covariates checks the assumptions?
covariates <- paste(colnames(exogenous), collapse = ' + ')
formulas <- c("toserve + suspend + probat", "toserve", "suspend", "probat", "incarcerate")
#ivmodel better?
ivregs <- lapply(setNames(formulas, formulas),
                 function(x) {
                   
    form <- glue::glue("laterarr ~ {x} | {calendars}")
    
    plm_fit <- plm(formula(form), data = X, model = "pooling",
               subset = incjudge == 1, index = c("clusterid"))
    
    plm_cov <- plm(formula(glue::glue("laterarr ~ {x} + {covariates} | {covariates} + {calendars}")),
                   data = X, model = "pooling", subset = incjudge == 1, index = c("clusterid"))
    
   cse <- coeftest(plm_fit, vcov=vcovHC(plm_cov, type="sss", cluster="group"))
   # doesn't have F statistic right now
   # no cse on plm_cov
    
    return(list(plm = summary(plm), plm_cov = plm_cov,
                cse = cse))
})

stargazer::stargazer(ivregs$toserve$plm_cov, output = "outreg_tsls_laterarr_toserve.png")

# table 7 #/  ## instrumental variables estimation, estimated using limited information maximum likelihood, with robust standard errors clustered on clusterid var.
## look into ivmodel package

ivregress <- function(endogenous, outcome = laterarr){
    # X must be attached
    # only one endogenous variable
    lapply(endogenous, function(en) {
        outreg <- ivmodel(Y = outcome,
                          D = X[[en]],
                          Z = as.factor(calendar),
                          X = exogenous,
                          k = 1, # 2SLS
                          manyweakSE = T,
                          clusterID = clusterid)
        selection <- lapply(c("LIML", "kClass"), function(element) {
            do.call(bind_cols, c(regression = element,
                                 endogenous = en,
                                 lapply(outreg[[element]][1:5], as.vector)))
        }) %>% reduce(bind_rows)
    
    }) %>% reduce(bind_rows)
}
endogenous <- c("probat", "toserve", "incarcerate", "probatnonzero")
table7 <- ivregress("toserve")
