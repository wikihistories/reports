# Import all data for report 1
# Run from main project directory
library(tidyverse, quietly = TRUE)
library(igraph, quietly = TRUE)
library(wikkitidy, quietly = TRUE)
source(file.path("scripts", "who-counts-lib.R"))

DATA_DIR <- file.path("data", "who-counts")

# Use cached versions of files?
USE_CACHE <- TRUE

# Download Wikipedia data
wikipedia_category_aus_people <- get_aussies_category_tree(USE_CACHE, DATA_DIR)
wikibase_items <- wikipedia_category_aus_people %>%
  pluck("nodes") %>%
  filter(ns == 0) %>%
  pull(pageid) %>%
  get_wikibase_items(USE_CACHE, DATA_DIR)

# Query Wikidata
wikidata_australians <- get_wikidata_australians(USE_CACHE, DATA_DIR)

# Join two datasets
combined_data <- wikibase_items %>%
  select(-c(ns:title)) %>%
  full_join(wikipedia_category_aus_people$nodes, by = "pageid") %>%
  rename(person = wikibase_item) %>%
  full_join(wikidata_australians, by = "person") %>%
  mutate(
    dataset = case_when(
      person %in% wikibase_items$wikibase_item & person %in% wikidata_australians$person ~ "both",
      person %in% wikibase_items$wikibase_item ~ "category_aus",
      .default = "wikidata"
    )
  )

# Get additional data
personal_data <- combined_data %>%
  filter(!is.na(person)) %>%
  get_personal_data(USE_CACHE, DATA_DIR)

full_dataset <- combined_data %>%
  left_join(personal_data, by = "person") %>%
  mutate(title = coalesce(title.x, title.y)) %>%
  select(-title.x, -title.y) %>%
  relocate(pageid, person, title)

# Get page history/assessment info for full dataset
# This can take a long time. If you need to do it over multiple sessions,
# be sure to set the 'resume' argument to TRUE after the first session,
# so that the csv isn't overwritten
quality_indicators <- get_quality_indicators(full_dataset, USE_CACHE, DATA_DIR)

# The quality indicators table should correspond exactly to the full_dataset,
# so the columns can simply be added.
full_dataset <- add_column(full_dataset, !!!quality_indicators)
