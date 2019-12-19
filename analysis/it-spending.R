library(skimr)

pservices_named_it <- pservices_named_vendors %>%
  filter(ROBJ_EN_NM == "Informatics Services")

pservices_named_it %>%
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
