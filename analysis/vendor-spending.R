pservices_named_vendors %>%
  filter(vendor_normalized == "EY") %>%
  group_by(fyear, ROBJ_EN_NM) %>% summarize(total = sum(AGRG_PYMT_AMT)) %>% ggplot(aes(x = fyear, y = total, fill = ROBJ_EN_NM)) + geom_col()