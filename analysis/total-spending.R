professional_services %>%
  group_by(fyear) %>%
  summarize(spend = sum(AGRG_PYMT_AMT)) %>%
  mutate(prop = spend / sum(spend)) %>%
  ggplot(aes(x = fyear, y = spend)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar)

professional_services %>%
  group_by(fyear) %>%
  summarize(spend = sum(AGRG_PYMT_AMT)) %>%
  mutate(prop = spend / sum(spend)) %>%
  ggplot(aes(x = fyear, y = prop)) +
  geom_col()

professional_services_spend_yoy <- professional_services %>%
  group_by(fyear) %>%
  summarize(spend = sum(AGRG_PYMT_AMT)) %>%
  mutate(change = spend / ((.) %>% slice(1) %>% pull(spend))) %>%
  mutate(grouping = "baseline")
