# Which Category:Australian people articles are biographies?
library(tidyverse)
library(wikkitidy)

BATCH_IDX <- 4L
BATCH_SIZE <- 10000L

lower <- BATCH_SIZE * (BATCH_IDX - 1) + 1
upper <- BATCH_SIZE * BATCH_IDX

wikidata_records <- readr::read_csv("kelly/australian_wikidata_records.csv") %>%
  mutate(
    title = stringr::str_remove(sitelink, "https://en.wikipedia.org/wiki/") %>% stringr::str_replace_all("_", " ") %>% URLdecode()
  )
wikipedia_australian_articles <- readr::read_rds("data/who-counts/australian-people-categories-1684481346.rds")
wikipedia_to_wikidata <- readr::read_csv("data/who-counts/australians-wikidata-items.csv")

has_human_property <- function(url) {
  response <- httr2::request(url) %>%
    httr2::req_error(is_error = function(x) FALSE) %>%
    httr2::req_perform()
  if (!httr2::resp_is_error(response)) {
    response_body <- httr2::resp_body_json(response)
    value <- purrr::pluck(response_body, "claims", "P31", 1, "mainsnak", "datavalue", "value", "id", .default = FALSE)
  } else {
    value <- FALSE
  }
  "Q5" %in% value
}

is_human <- function(wikidata_id) {
  url <- paste0(
    "https://www.wikidata.org/w/api.php?action=wbgetclaims&entity=",
    wikidata_id,
    "&property=P31&props=&format=json"
  )
  map_lgl(url, has_human_property, .progress = T)
}

combined_data <- wikipedia_australian_articles$nodes %>%
  filter(ns == 0) %>%
  full_join(wikidata_records, by = "title")

wikipedia_biographies <- combined_data %>%
  filter(
    is.na(person), # all articles not in Wikidataset
    !str_detect(title, "Lists?\\b"),
    !str_detect(title, "Australia[isn']{0,2}\\b")
  ) %>%
  select(pageid, title) %>%
  left_join(
    select(wikipedia_to_wikidata, pageid, wikibase_item),
    by = "pageid"
  ) %>%
  drop_na(wikibase_item) %>%
  slice(lower:upper) %>%
  mutate(
    biography = is_human(wikibase_item)
  )

readr::write_csv(wikipedia_biographies, glue::glue("data/who-counts/category-australia-biographies-{lower}-to-{upper}.csv"))
