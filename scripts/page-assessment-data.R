# Run from root 'reports' directory of project
library(tidyverse)
library(wikkitidy)

BATCH_IDX <- 10L # increment by 1 to get next batch
if (BATCH_IDX < 1 || !is.integer(BATCH_IDX)) {
  stop("BATCH_IDX must be a positive integer")
}
BATCH_SIZE <- 10000L

lower <- BATCH_SIZE * (BATCH_IDX - 1) + 1
upper <- BATCH_SIZE * BATCH_IDX

wikidata_records <- readr::read_csv("kelly/australian_wikidata_records.csv") %>%
  mutate(
    title = stringr::str_remove(sitelink, "https://en.wikipedia.org/wiki/") %>% stringr::str_replace_all("_", " ") %>% URLdecode()
  )
wikipedia_australian_articles <- readr::read_rds("data/who-counts/australian-people-categories-1684481346.rds")

combined_data <- wikipedia_australian_articles$nodes %>%
  filter(ns == 0) %>%
  full_join(wikidata_records, by = "title")

with_assessments <- combined_data %>%
  slice(lower:upper) %>%
  mutate(get_xtools_page_info(title, failure_mode = "quiet"))

readr::write_csv(with_assessments, glue::glue("data/who-counts/combined-assessed-data-rows-{lower}-to-{upper}.csv"))

