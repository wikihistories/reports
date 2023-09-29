# Combine different tables into single one
library(tidyverse)
DATA_DIR <- file.path("data", "who-counts")

import_split_file <- function(directory, pattern, extension = c("csv","rds")) {
  read_func <- switch(rlang::arg_match(extension),
                      "csv" = read_csv,
                      "rds" = read_rds)
  withr::with_options(list("readr.show_col_types" = FALSE), {
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

missing_details <- import_split_file(DATA_DIR, "personal-data-for-biography") %>%
  mutate(across(dob:dod, as_date))

complete_data <- assessed_data %>%
  # Filter out non-biographies from Wikipedia dataset
  left_join(
    select(which_biographies, -title),
    by = "pageid",
    keep = TRUE,
    ) %>%
  mutate(biography = biography | !is.na(person)) %>%
  # Add missing data for dob etc. for Wikipedia dataset
  left_join(
    select(missing_details, -title),
    by = join_by("pageid", "wikibase_item", "biography"),
    keep = TRUE,
  ) %>%
  mutate(
    genderLabel = coalesce(genderLabel, gender),
    dob = coalesce(dob.x, dob.y),
    dod = coalesce(dod.x, dod.y),
    pobLabel = coalesce(pobLabel, pob)
  ) %>%
  select(-gender, -ends_with(".x"), -ends_with(".y"), -pob)

write_rds(complete_data, file.path(DATA_DIR, "complete-data.rds"), compress = "gz")
