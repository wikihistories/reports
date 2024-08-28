# Construct dataset for 2024 annual report
source(file.path("scripts", "which-places-lib.R"))

USE_CACHE <- TRUE
DATA_DIR <- file.path("data", "which-places")
PLACE_FILE <- file.path(DATA_DIR, "places.csv")
TYPE_FILE <- file.path(DATA_DIR, "place_types.csv")

# Main dataset
places <- load_places(out_path = PLACE_FILE, use_cache = USE_CACHE)

# Auxiliary tables
types <- load_types(TYPE_FILE, places, use_cache = USE_CACHE)
lang_codes <- get_language_codes()






