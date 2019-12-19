## to use when populating indices (manually, of course!)
lword <- pservices_named_vendors %>%
  mutate(lword = word(PRJCT_EN_DESC, -1))


# Use this to clean up once you think you're done:
lword %>%
  select(lword) %>%
  anti_join(canadian_regions_index, by = c("lword" = "region_lword")) %>%
  count_group(lword) %>%
  View()

# Want to verify that lword reliably indicates the province in question.
# e.g. with "Territories", which we think means NWT...
lword %>%
  filter(lword == "Territories") %>%
  select(PRJCT_EN_DESC) %>%
  filter(! str_detect(PRJCT_EN_DESC, "Northwest"))
# ... should return 0. [TODO: Use assert or some such. Y'know.]