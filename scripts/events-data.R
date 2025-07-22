# Import data for report 3
# Run from main project directory
library(tidyverse, quietly = TRUE)
library(igraph, quietly = TRUE)
library(wikkitidy, quietly = TRUE)
source(file.path("scripts", "events-lib.R"))

DATA_DIR <- file.path("data", "events")

# Use cached versions of files?
USE_CACHE <- TRUE

OCCURENCES_FILE <- file.path(DATA_DIR, "occurrences.rds")

# Import
occurrences <- load_occurrences(out_path = OCCURENCES_FILE, use_cache = FALSE)
