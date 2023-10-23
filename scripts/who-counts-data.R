# Import all data for report 1
# Run from main project directory
library(tidyverse, quietly = TRUE)
library(igraph, quietly = TRUE)
library(wikkitidy, quietly = TRUE)
source(file.path("scripts", "who-counts-lib.R"))

DATA_DIR <- file.path("data", "who-counts")

# Use cached versions of files?
USE_CACHE <- FALSE

# Download two datasets
wikipedia_category_aus_people <- get_aussies_category_tree(USE_CACHE, DATA_DIR)
wikidata_australians <- get_wikidata_australians(USE_CACHE, DATA_DIR)

# Join two datasets
combined_data <- wikipedia_category_aus_people$nodes %>%
  filter(ns == 0) %>%
  mutate(person = get_wikibase_items(pageid, USE_CACHE, DATA_DIR) %>% pull(wikibase_item)) %>%
  combine_datasets(wikidata_australians)

# Annotate data with gender, pob etc.
annotated_combined_data <- annotate_combined_data(combined_data, USE_CACHE, DATA_DIR)

# Download page quality indicators from XTools
