plot_vendor_spend_by_fy <- function(spends_by_vendor = pservices_named_vendors, vendor_to_plot) {
  spends_by_vendor %>%
    filter(vendor_normalized == vendor_to_plot) %>%
    group_by(fyear, ROBJ_EN_NM) %>%
    summarize(total = sum(AGRG_PYMT_AMT)) %>%
    ggplot(aes(x = fyear, y = total, fill = ROBJ_EN_NM)) +
    geom_col()
}

pservices_named_vendors %>%
  plot_vendor_spend_by_fy("IBM")

pservices_named_it %>%
  plot_vendor_spend_by_fy("IBM")
