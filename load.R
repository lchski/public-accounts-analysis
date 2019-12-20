library(tidyverse)
library(lubridate)

source("scripts/helpers.R")

canadian_regions_index <- read_csv("data/indices/canadian-regions.csv")
vendors_index <- read_csv("data/indices/vendors.csv")

## Data from: https://open.canada.ca/data/en/dataset/ac597ff8-ee13-48c3-b315-42e528090af2
professional_services <- tibble(path = fs::dir_ls("data/source/", regexp = "\\.csv$")) %>%
  pull(path) %>%
  map_dfr(
    read_csv,
    col_types = cols("MINC" = col_double(), "ROBJ_CD" = col_double(), `DepartmentNumber-Numéro-de-Ministère` = col_character())
  ) %>%
  filter(! is.na(PRJCT_EN_DESC)) %>% ## remove rows that just total categories, we can do that ourselves
  remove_extra_columns() %>%
  mutate(PRJCT_EN_DESC = trimws(PRJCT_EN_DESC, whitespace = "[\\h\\v]")) %>%
  mutate(fyear = str_trunc(FSCL_YR, 4, ellipsis = "")) %>%
  mutate(fyear = parse_date(paste0(fyear, "-04-01")))

pservices_anonymized_vendors <- professional_services %>%
  filter(str_detect(PRJCT_EN_DESC, regex("Service payments under|Service paiements under|Services payments under|Service payment under|Service payments over", ignore_case = TRUE)))

pservices_named_vendors <- professional_services %>%
  filter(! str_detect(PRJCT_EN_DESC, regex("Service payments under|Service paiements under|Services payments under|Service payment under|Service payments over", ignore_case = TRUE))) %>%
  mutate(lword = word(PRJCT_EN_DESC, -1)) %>%
  left_join(canadian_regions_index, by = c("lword" = "region_lword")) %>%
  mutate(region_normalized = case_when( ## Fixing exceptions noted in region notes.
    (lword == "Columbia" || lword == "Colombia") & str_detect(PRJCT_EN_DESC, "District of Columbia|Bogota") ~ NA_character_,
    TRUE ~ region_normalized
  )) %>%
  mutate(is_canadian_vendor = ! is.na(region_normalized)) %>%
  mutate(
    vendor_identifier = case_when(
      is_identified_vendor(PRJCT_EN_DESC) ~ str_to_lower(str_extract(PRJCT_EN_DESC, regex(
        paste0("^", paste(collapse = "|^", vendors_index %>% pull(vendor_signal))),
        ignore_case = TRUE
      ))),
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    vendors_index %>%
      mutate(vendor_signal = str_to_lower(vendor_signal)),
    by = c("vendor_identifier" = "vendor_signal")
  )
