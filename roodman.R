library(tidyverse)
library(AER)
library(ivmodel)
X <- readr::read_csv("data/green_winik_data.csv")
attach(X)
N <- nrow(X)



end <- sort(rep(seq(2, 365*4, 1), N))

dated <- X %>% mutate(across(ends_with("date"), ~as.Date(., "%m/%d/%Y"), .names = "{col}_"))

expanded <- do.call("rbind", replicate(length(unique(end)), dated, simplify = FALSE)) %>%
  mutate(end = end,
         laterarr2 = ifelse(is.na(sentdate_) | laterarrdate == "0", 0,
                            (sentdate_ + end) > laterarrdate_))
change_index <- expanded %>% count(end, laterarr2) %>% filter(laterarr2 == 0) %>% group_by(n) %>%
  mutate(id = cur_group_id()) %>%
  summarize(end = max(end), n = n()) %>% arrange(end)

listed <- expanded %>% group_by(end) %>% group_split
x <- listed[[1]]

library(doParallel)
registerDoParallel(16)
before <- Sys.time()

out <- map2(.x = listed[(change_index$end - 1)[1:10]],
     .y = change_index$n[1:10],
     .f = function(x, n) {
  outreg <- with(x, liml(Y = laterarr2, D = toserve, Z = as.factor(calendar),
                         k = 1,
                         manyweakSE = T, clusterID = clusterid))
  do.call(bind_cols, lapply(outreg[1:5], as.vector)) %>% replicate(n, ., simplify = F)
}) %>% reduce(bind_rows)
regressed <- out %>% bind_cols(end = unique(expanded$end)[1:nrow(out)])

after <- Sys.time()
after-before


regressed %>% ggplot(aes(x = end, y = cumsum(point.est))) + geom_line()

out <- lapply(listed[1:10], function(x) {
  outreg <- with(x, liml(Y = laterarr, D = toserve, Z = as.factor(calendar),
                         k = 1, # 2SLS
                         manyweakSE = T, clusterID = clusterid))
  
  do.call(bind_cols, c(period = unique(x$end), lapply(outreg[1:5], as.vector)))
}) %>% reduce(bind_rows)


sort(rep(seq(2, 365*4, 1), N))
varied <- lapply(seq(2, 365*4, 1)[1:10], function(end) {
  new <- X %>% mutate(across(ends_with("date"), ~as.Date(., "%m/%d/%Y"),
                             .names = "{col}_as"),
                      end = end,
                      laterarr2 = ifelse(is.na(sentdate_as) | laterarrdate == "0", 0,
                              (sentdate_as + end) > laterarrdate_as))
  ## check output
  #new %>%
  #  mutate(range = sentdate_as + end, test = range > laterarrdate_as) %>%
  #  count(laterarr, laterarr2, laterarrdate_as,
  #              range, test, sentdate_as, end)
  dummy <- capture.output(ivmodel)
  before <- Sys.time()
  outreg <- with(new, capture.output(ivmodel(Y = laterarr, D = toserve, Z = as.factor(calendar),
                              k = 1, # 2SLS
                              manyweakSE = T, clusterID = clusterid)))
  after <- Sys.time()
  print(after - before)
  
  #do.call(bind_cols, c(period = end,
  #                     lapply(outreg$LIML[1:5], as.vector)))
}) #%>% reduce(bind_rows)
after <- Sys.time()

print(glue::glue("{after - before}"))

