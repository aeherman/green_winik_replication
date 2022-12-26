source("start_up.R")

# rankings plot
rankings <- X %>% select(calendar, all_of(endogenous)) %>%
  group_by(calendar) %>% summarize(across(all_of(endogenous), ~signif(mean(.), 3))) %>%
  mutate(across(all_of(endogenous), ~signif(./max(.), 3))) %>%
  pivot_longer(all_of(endogenous), names_to = "en_var", values_to = "length") %>%
  group_by(en_var) %>% arrange(en_var, length) %>% mutate(rank = row_number()) %>%
  ggplot(aes(x = en_var, y = length, group = calendar, color = as.factor(calendar))) +
  geom_point() + geom_path(show.legend = F)  +
  ylab("Harshness Ranking\n") + xlab("Sentence Severity Measures") +
  ggrepel::geom_text_repel(aes(label = ifelse(en_var == "toserve", calendar, NA_character_)),
            show.legend = F, nudge_x = .3, segment.lty = "dotted", alpha = 0.5) +
  ggtitle("Metric disagreement in ranking calendars") +
  theme(legend.position = "none")
rankings

outliers <- X %>% filter(calendar == 2) %>% select(calendar, incarc) %>%
  #pivot_longer(c("incarc", "toserve"), names_to = "sentence", values_to = "length") %>%
  arrange(desc(incarc))
outliers %>% count(incarc) %>% arrange(desc(incarc))
outliers %>% filter(length >= 50, !(sentence == "incarc" & length < 300)) %>% count(calendar, sentence, length)

outliersp <- outliers %>%
  ggplot( fill = "blue") +
  geom_density(aes(x = incarc), alpha = 0.5) +
  ggtitle("Density distribution of 'incarc' in calendar 2") + xlab("\n Months of Incarceration Sentence") #+
  #geom_point(aes(x = 324, y = 23), inherit.aes = F)


longer <- X %>% mutate(disp_yearmon = as.yearmon(dispdate_)) %>%
  group_by(calendar, disp_yearmon) %>% summarize(across(all_of(endogenous), ~mean(.))) %>%
  pivot_longer(all_of(endogenous), names_to = "sentence", values_to = "length")

longer_outliers <- X %>% mutate(disp_yearmon = as.yearmon(dispdate_)) %>%
  filter(!(calendar == 2 & incarc == 324)) %>%
  group_by(calendar, disp_yearmon) %>% summarize(across(all_of(endogenous), ~mean(.))) %>%
  pivot_longer(all_of(endogenous), names_to = "sentence", values_to = "length")
  
# as many as four per day
adf <- lapply(unique(longer$sentence), function(type) {
  out <- adf.test(longer$length[longer$sentence == type])
  bind_cols(variable = type, out[c(1, 4, 3)])
}) %>% reduce(bind_rows)
adf

ts <- longer %>% 
  ggplot(aes(x = disp_yearmon, y = length, color = as.factor(calendar))) +
  
  geom_vline(xintercept = c(as.yearmon(c("Jan 2003", "Jan 2004"))), lty = "dotted") +
  geom_smooth(aes(fill = as.factor(calendar)), lty = "dotted", alpha = 0.2, method = "glm") +
  facet_wrap(sentence ~ ., scales = "free") +
  ggtitle("Sentencing behaviors over time for each measure of harshness") +
  ylab("Mean Value\n") + xlab("\nMonth") + theme(legend.position = "bottom")

ts_incarc <- longer_outliers %>% filter(sentence == "incarc") %>%
  ggplot(aes(x = disp_yearmon, y = length, color = as.factor(calendar))) +
  
  geom_vline(xintercept = c(as.yearmon(c("Jan 2003", "Jan 2004"))), lty = "dotted") +
  geom_smooth(aes(fill = as.factor(calendar)), lty = "dotted", alpha = 0.2, method = "glm") +
  ggtitle("Sentencing behaviors over time for each measure of harshness",
          subtitle = "23 outlying observations of 324 months removed") +
  ylab("Mean Value\n") + xlab("\nMonth") + theme(legend.position = "bottom")


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
