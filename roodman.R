source("start_up.R")

# varied end dates for measuring follow-up period
end <- sort(rep(seq(2, 365*4, 1), N))

# transform date columns to date values
dated <- X %>% mutate(across(ends_with("date"), ~as.Date(., "%m/%d/%Y"), .names = "{col}_"))

# make all the variables needed, and repeat the dataframe for each defined range
expanded <- do.call("rbind", replicate(length(unique(end)), dated, simplify = FALSE)) %>%
  mutate(end = end,
         laterarr2 = ifelse(is.na(sentdate_) | laterarrdate == "0", 0,
                            (sentdate_ + end) > laterarrdate_))

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
  geom_hline(aes(yintercept = 0))

date <- str_replace_all(Sys.Date(), pattern = "-", replacement = "_")
saveRDS(regressed, file = glue::glue("data/replicated/vary_range_any_arrest_{date}.rds"))
