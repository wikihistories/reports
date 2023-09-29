# Get extract for all biographies in dataset
library(tidyverse)
library(wikkitidy)
DATA_DIR <- file.path("data", "who-counts")

complete_data <- file.path(DATA_DIR, "complete-data.rds") %>%
  read_rds() %>%
  filter(biography | !is.na(sitelink))

get_extract <- function(title) {
  if (length(title) > 20) {
    rlang::abort("Too many titles. You can only retrieve 20 extracts at a time")
  }
  response <- wiki_action_request() %>%
    query_by_title(title) %>%
    query_page_properties(
      "extracts",
      exintro = TRUE, # just get intro
      explaintext = TRUE, # get plain text rather than html
      exlimit = 20 # you can only get 20 at a time!
      ) %>%
    retrieve_all()
  response
}

split_batches <- function(variable, batch_size) {
  batches <- split(variable, ceiling(seq_along(variable)/batch_size)) %>%
    map(\(x) paste0(x, collapse = "|"))
  batches
}

get_all_extracts <- function(title) {
  batches <- split_batches(title, 20)
  extracts <- map(batches, get_extract, .progress = "Getting intros")
  bind_rows(extracts)
}

extracts <- complete_data %>%
  pull(title) %>%
  get_all_extracts() %>%
  write_rds(file.path(DATA_DIR, "page-intros.rds"))
