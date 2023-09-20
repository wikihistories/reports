# Combine different tables into single one
library(tidyverse)
DATA_DIR <- file.path("data", "who-counts")

import_split_file <- function(directory, pattern, extension = c("csv","rds")) {
  read_func <- switch(rlang::arg_match(extension),
                      "csv" = read_csv,
                      "rds" = read_rds)
  withr::with_options("readr.show_col_types" = FALSE, {
    data <- tibble(
      path = list.files(directory)
    ) %>%
      filter(str_starts(path, pattern)) %>%
      mutate(path = file.path(DATA_DIR, path)) %>%
      pull(path) %>%
      map(read_func) %>%
      bind_rows()
  })
  data
}

assessed_data <- import_split_file(DATA_DIR, "combined-assessed-data")

which_biographies <- import_split_file(DATA_DIR, "category-australia-biographies")

missing_details <- import_split_file(DATA_DIR, "personal-data-for-biography", "rds")
