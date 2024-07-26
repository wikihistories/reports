# Construct dataset for 2024 annual report
source(file.path("scripts", "which-places-lib.R"))

USE_CACHE <- TRUE
DATA_DIR <- file.path("data", "which-places")
PLACE_FILE <- file.path(DATA_DIR, "places.csv")
TYPE_FILE <- file.path(DATA_DIR, "place_types.csv")

places <- load_places(out_path = PLACE_FILE, use_cache = USE_CACHE)
types <- load_types(TYPE_FILE, places, use_cache = USE_CACHE)

library(tidyverse)
library(gridExtra)
places <- read_csv("data/which-places/places.csv")
types <- read_csv("data/which-places/place_types.csv")






