require(scales)

plot_vendor_spend_by_fy_by_robj <- function(spends_by_vendor = pservices_named_vendors, vendor_to_plot) {
  spends_by_vendor %>%
    filter(vendor_normalized == vendor_to_plot) %>%
    group_by(fyear, ROBJ_EN_NM) %>%
    summarize(total = sum(AGRG_PYMT_AMT)) %>%
    ggplot(aes(x = fyear, y = total, fill = ROBJ_EN_NM)) +
    geom_col() +
    scale_y_continuous(labels = scales::dollar) +
    labs(title = paste0("GC payments to ", vendor_to_plot, ", by object code"))
}

plot_vendor_spend_by_fy_by_mine <- function(spends_by_vendor = pservices_named_vendors, vendor_to_plot) {
  spends_by_vendor %>%
    filter(vendor_normalized == vendor_to_plot) %>%
    group_by(fyear, MINE) %>%
    summarize(total = sum(AGRG_PYMT_AMT)) %>%
    ggplot(aes(x = fyear, y = total, fill = MINE)) +
    geom_col()
}

compare_vendor_to_baseline <- function(spends_by_vendor = pservices_named_vendors, vendor_to_plot) {
  spends_by_vendor %>%
    filter(vendor_normalized == vendor_to_plot) %>%
    group_by(fyear) %>%
    summarize(spend = sum(AGRG_PYMT_AMT)) %>%
    mutate(change = spend / ((.) %>% slice(1) %>% pull(spend))) %>%
    mutate(grouping = vendor_to_plot) %>%
    bind_rows(professional_services_spend_yoy)
}

pservices_named_vendors %>%
  plot_vendor_spend_by_fy_by_robj("Gartner")

pservices_named_vendors %>%
  compare_vendor_to_baseline("Gartner") %>%
  ggplot(aes(x = fyear, y = change, color = grouping)) +
  geom_point() +
  geom_line()


pservices_named_vendors %>%
  filter(ROBJ_EN_NM == "Informatics services") %>%
  plot_vendor_spend_by_fy_by_robj("Veritaaq Technology House")

pservices_named_vendors %>%
  plot_vendor_spend_by_fy_by_mine("Deloitte")
