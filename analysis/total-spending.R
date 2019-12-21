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
