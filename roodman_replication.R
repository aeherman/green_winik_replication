source("start_up.R")

# varied end dates for measuring follow-up period
end <- sort(rep(seq(2, 365*4, 1), N))

# transform date columns to date values
dated <- X %>% mutate(across(ends_with("date"), ~as.Date(., "%m/%d/%Y"), .names = "{col}_"))

# make all the variables needed, and repeat the dataframe for each defined range
expanded <- do.call("rbind", replicate(length(unique(end)), dated, simplify = FALSE)) %>%
  mutate(end = end,
         laterarr2 = ifelse(is.na(dispdate_) | laterarrdate == "0", 0,
                            (dispdate_ + end) >= laterarrdate_))
# filter for unique values for each defined range
# in order to minimize calculation time in the next step
# (total "laterarr2" each time the range is increased by a day)
change_index <- expanded %>% count(end, laterarr2) %>% filter(laterarr2 == 0) %>% group_by(n) %>%
  mutate(id = cur_group_id()) %>%
  # maybe try using the min instead?  but the min isn't working.
  # attempt a plot to visualize what this means
  summarize(end = max(end), min_end = min(end), n = n()) %>% arrange(end)

change_index %>%
  filter(end < 50) %>%
  ggplot(aes(x = end, y = n)) + geom_step()

listed <- expanded %>% group_by(end) %>% group_split

library(doParallel)
registerDoParallel(16)

before <- Sys.time()
out <- map2(.x = listed[(change_index$end - 1)], # unique versions of the data given the above
     .y = change_index$n, # keep track of how many days the cumulative laterarr2 remains the same
     .f = function(x, n) {
  outreg <- with(x, liml(Y = laterarr2, D = toserve, Z = as.factor(calendar),
                         # consider adding in the exogenous variables with D
                         X = exogenous,
                         k = 1,
                         manyweakSE = T, clusterID = clusterid))
  do.call(bind_cols, lapply(outreg[1:5], as.vector)) %>%
    # repeat the vari
    replicate(n, ., simplify = F) # repeat the results for each day it was the same
}) %>% reduce(bind_rows)
regressed <- out %>%
  # add back in the associated range of days
  bind_cols(end = unique(expanded$end)[1:nrow(out)])

after <- Sys.time()
after-before

freed <- expanded %>%
  transmute(end, incarcerate, dispdate_, toserve, suspend,
            reldate_ = dispdate_ + toserve*30, # estimated, not correct for everything
            enddate_ = dispdate_ + end, # end of the period of observation
            laterarrdate_,
            free = fullreleasetorecid == 9999 & toserve == 0  |
              reldate_ <= enddate_ & enddate_ < laterarrdate_ & !is.na(laterarrdate_) |
              reldate_ <= enddate_ & is.na(laterarrdate_),
            fullreleasetorecid) %>% group_by(end, incarcerate) %>%
  summarize(free = sum(free)/N) #%>% filter(incarcerate == 1)

regressed %>% ggplot() +
  geom_ribbon(aes(x = end,
                  ymin = point.est - 1.96*std.err,
                  ymax = point.est + 1.96*std.err),
            alpha = .5, fill = "lightgreen") +
  geom_ribbon(aes(x = end,
                  ymin = point.est - std.err,
                  ymax = point.est + std.err),
              alpha = .6, fill = "green") +
  geom_line(aes(x = end, y = point.est), color = "darkgreen") +
  geom_hline(aes(yintercept = 0), color = "white") +
  #geom_line(data = freed, aes(x = end, y = free), inherit.aes = F) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-.08, .05, .01)) +
  ylab("Cumulative Recidivism (rearrest rate)\n") +
  xlab("\nFollow-up Interval\n(days since a defendant's initial disposition)") +
  ggtitle("Marginal impact of increasing follow-up interval on\nestimating the marginal effect of\n1 month of incarceration on recidivism")

freed %>% ggplot(aes(x = end, y = free,
                     color = ifelse(incarcerate == 1,
                                    "incarcerated or\narrested",
                                    "released"))) +
  geom_line() +
  ggtitle("Proportion of incarcerated subjects released\nin a given follow-up interval") +
  ylab("Proportion Free or Released\n") +
  xlab("\nFollow-up Interval\n(days since a defendent's initial disposition)") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_discrete(NULL)

# question: is toserve not the orginal sentence?
## suspension is part of the original sentencing
latercondate_




freed %>% ggplot(aes(x = end, y = free)) + geom_line()

freed %>% filter(end > 500)

# question: on what date does the defendant start serving their sentence?
# fullreleasetorecid = laterarrdate_ - dispdate_ - 30*toserve
# a couple people have estimated release dates that extend beyond their later arrest dates
## I will filter them out or just include them
mean((dispdate_ + toserve) < laterarrdate_, na.rm = T)
X %>% filter((dispdate_ + toserve) >= laterarrdate_) %>% select(dispdate_, toserve, laterarrdate_, suspend)

X %>% filter(toserve/12<4) %>%
  mutate(recid = as.numeric(laterarrdate_ - dispdate_) - 30*toserve,
         fullreleasetorecid = fullreleasetorecid) %>%
  select(dispdate_, sentdate_, toserve, suspend, laterarr, laterarrdate_, fullreleasetorecid,
                                      recid, incarcerate)

free = case_when(
  fullreleasetorecid == 999 & toserve == 0 ~ T, # no incarceration
  
)

freedate_ <- dispdate_ + toserve

count(X, toserve == 0, incarcerate == 0)

X %>% group_by(isnasentdate = is.na(sentdate_), iszerotoserve = toserve == 0) %>%
  filter(isnasentdate, !iszerotoserve) %>% select(sentdate_, dispdate_, toserve, fullreleasetorecid, suspend)


date <- str_replace_all(Sys.Date(), pattern = "-", replacement = "_")
saveRDS(regressed, file = glue::glue("data/replicated/vary_range_any_arrest_{date}.rds"))



out2 <- map2(.x = listed[(change_index$end - 1)], # unique versions of the data given the above
            .y = change_index$n, # keep track of how many days the cumulative laterarr2 remains the same
            .f = function(x, n) {
              
              outplm <- plm(formula(glue::glue("laterarr2 ~ toserve + {covariates} |
                              {covariates} + {calendars}")),
                            data = x, model = "pooling", subset = incjudge == 1,
                            index = c("clusterid"))
              cse <- coeftest(outplm, vcov=vcovHC(outplm, type="sss", cluster="group"))
              do.call(bind_cols, lapply(outreg[1:5], as.vector)) %>%
                # repeat the vari
                replicate(n, ., simplify = F) # repeat the results for each day it was the same
            }) %>% reduce(bind_rows)
regressed <- out %>%
  # add back in the associated range of days
  bind_cols(end = unique(expanded$end)[1:nrow(out)])


# anderson-rubin test

library(boot)
#boot(X, statistic = with(data = X, ivmodel(Y = laterarr, D = toserve, Z = as.factor(calendar), k = 1, clusterID = clusterid)), 2)

outreg <- ivmodel(Y = X$laterarr, D = X$toserve, Z = as.factor(rank_toserve), #X = exogenous,
                  k = 0, clusterID = clusterid)

outreg$kClass
ARsens.power(n = nrow(X), # sample size
             k = ncol(exogenous), # exogenous variables
             beta = 1,
             gamma = outreg$LIML$point.est, # coefficient
             Zadj_sq = var(rank_toserve), # question
             sigmau = outreg$LIML$std.err, #
             rho = .5, # assumption
             sigmav = outreg$kClass$std.err, # first stage SE, regular OLS when k = 0
             alpha = 0.05, deltarange = c(-0.5, 0.5)
             )


point.est <- lapply(outreg$LIML, as.vector)[c("point.est", "p.value")] %>% reduce(cbind)
lapply(seq(-0.5, 0.5, 0.01), function(x) {
  output <- AR.test(outreg, beta0 = x)
  tibble(h0 = x, p.value = output$p.value, cil = output$ci[1], ciu = output$ci[2])}
  ) %>%
  reduce(rbind) %>%
  ggplot() +
  #geom_ribbon(aes(x = h0, ymin = cil, ymax = ciu)) + # the ci of the original point estimate
  geom_line(aes(x = h0, y = p.value)) +
  geom_hline(aes(yintercept = 0.05), color = "red") +
  geom_vline(aes(xintercept = point.est[1]), color = "green") +
  geom_vline(aes(xintercept = 0))

# very confidently reject that the coefficient is negative
X$incarc