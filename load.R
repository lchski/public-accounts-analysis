library(tidyverse)
library(lubridate)

source("scripts/helpers.R")

## Data from: https://open.canada.ca/data/en/dataset/ac597ff8-ee13-48c3-b315-42e528090af2
professional_services <- tibble(path = fs::dir_ls("data/source/", regexp = "\\.csv$")) %>%
  pull(path) %>%
  map_dfr(
    read_csv,
    col_types = cols("MINC" = col_double(), "ROBJ_CD" = col_double())
  ) %>%
  filter(! is.na(PRJCT_EN_DESC)) %>% ## remove rows that just total categories, we can do that ourselves
  remove_extra_columns() %>%
  mutate(PRJCT_EN_DESC = trimws(PRJCT_EN_DESC, whitespace = "[\\h\\v]"))

pservices_anonymized_vendors <- professional_services %>%
  filter(str_detect(PRJCT_EN_DESC, regex("^Service payments under", ignore_case = TRUE)))

pservices_named_vendors <- professional_services %>%
  filter(! str_detect(PRJCT_EN_DESC, regex("^Service payments under", ignore_case = TRUE)))


## to builduse when populating indices (manually, of course!)
lword <- pservices_named_vendors %>%
  mutate(lword = word(PRJCT_EN_DESC, -1))
