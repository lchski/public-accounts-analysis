source("lib/similarity.R")

library(lubridate)
library(tidytext)
library(exploratory)
library(SnowballC)

data("stop_words")
stop_words <- stop_words

vendors_to_compare <- pservices_named_vendors %>%
  select(text = PRJCT_EN_DESC) %>%
  count_group(text) %>%
  mutate(id = row_number())

compared_vendors <- analyse_statement_similarity(
  statements = vendors_to_compare,
  similarity_threshold = 0.75
)

## Compare text of similar statements
similar_vendors <- compared_vendors$above_threshold %>%
  mutate(id.x = as.integer(id.x), id.y = as.integer(id.y)) %>%
  left_join(vendors_to_compare %>%
              select(id, text), by = c("id.x" = "id")
  ) %>%
  rename(text.x = text) %>%
  left_join(vendors_to_compare %>%
              select(id, text), by = c("id.y" = "id")
  ) %>%
  rename(text.y = text) %>%
  arrange(id.x)

is_identified_vendor <- function(vendors) {
  str_detect(
    string = vendors,
    pattern = regex(
      paste(collapse = "|", vendors_index %>% pull(vendor_signal)),
      ignore_case = TRUE
    )
  )
}


similar_vendors %>%
  mutate(
    text.x.ident = is_identified_vendor(text.x),
    text.y.ident = is_identified_vendor(text.y)
  ) %>%
  filter(! text.x.ident | ! text.y.ident) %>%
  View("similar_vendors")

source("load.R")
similar_vendors %>%
  filter(
    text.x %in% (pservices_named_vendors %>% filter(ROBJ_EN_NM == "Informatics Services") %>% pull(PRJCT_EN_DESC) %>% unique()) |
    text.y %in% (pservices_named_vendors %>% filter(ROBJ_EN_NM == "Informatics Services") %>% pull(PRJCT_EN_DESC) %>% unique())
  ) %>%
  mutate(
    text.x.ident = is_identified_vendor(text.x),
    text.y.ident = is_identified_vendor(text.y)
  ) %>%
  filter(! text.x.ident | ! text.y.ident) %>%
  View("similar_vendors_in_it")
