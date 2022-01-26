library(skimr)

pservices_named_it <- pservices_named_vendors %>%
  filter(ROBJ_EN_NM == "Informatics services")

pservices_named_vendors %>%
  filter(ROBJ_EN_NM == "Informatics services") %>%
  select(vendor_normalized, PRJCT_EN_DESC, AGRG_PYMT_AMT) %>%
  group_by(vendor_normalized, PRJCT_EN_DESC) %>%
  summarize(
    count = n(),
    min = min(AGRG_PYMT_AMT),
    max = max(AGRG_PYMT_AMT),
    total = sum(AGRG_PYMT_AMT)
  ) %>%
  ungroup() %>%
  mutate(
    total_prop = total / sum(total),
    count_prop = count / sum(count)
  ) %>%
  select(vendor_normalized, PRJCT_EN_DESC, count, count_prop, min:total_prop) %>%
  arrange(-total_prop) %>%
  filter(is.na(vendor_normalized)) %>%
  filter(total_prop > 0.001)

pservices_named_it %>%
  select(vendor_normalized, AGRG_PYMT_AMT) %>%
  group_by(vendor_normalized) %>%
  summarize(
    count = n(),
    min = min(AGRG_PYMT_AMT),
    max = max(AGRG_PYMT_AMT),
    total = sum(AGRG_PYMT_AMT)
  ) %>%
  mutate(
    total_prop = total / sum(total),
    count_prop = count / sum(count)
  ) %>%
  select(vendor_normalized, count, count_prop, min:total_prop) %>%
  arrange(-total_prop)

professional_services %>%
  group_by(FSCL_YR, ROBJ_EN_NM) %>%
  summarize(spend = sum(AGRG_PYMT_AMT)) %>%
  mutate(spend_prop = spend / sum(spend)) %>%
  mutate(it_spend = ROBJ_EN_NM == "Informatics services") %>%
  group_by(FSCL_YR, it_spend) %>%
  summarize(spend_prop = sum(spend_prop)) %>%
  ggplot(aes(x = FSCL_YR, y = spend_prop, color = it_spend, fill = it_spend)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


## how much has spending on informatics increased year-over-year vs on pservices in general?
professional_services %>%
  filter(ROBJ_EN_NM == "Informatics services") %>%
  group_by(fyear) %>%
  summarize(spend = sum(AGRG_PYMT_AMT)) %>%
  mutate(change = spend / ((.) %>% slice(1) %>% pull(spend))) %>%
  mutate(grouping = "informatics") %>%
  bind_rows(professional_services_spend_yoy) %>% ## from `vendor-spending.R`
  ggplot(aes(x = fyear, y = change, color = grouping)) +
  geom_point() +
  geom_line() +
  ylim(c(0, 10))
