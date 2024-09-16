# Construct dataset for 2024 annual report
source(file.path("scripts", "which-places-lib.R"))

# Use saved data, or download afresh?
USE_CACHE <- TRUE

# Where the data is
DATA_DIR <- file.path("data", "which-places")
PLACE_FILE <- file.path(DATA_DIR, "places.rds")
TYPE_FILE <- file.path(DATA_DIR, "place_types.csv")
EDIT_FILE <- file.path(DATA_DIR, "edits.rds")
PAGEVIEWS_FILE <- file.path(DATA_DIR, "pageviews.csv")

# NB: the "load_" functions below all add a metadata attribute, "time_created"
# to the loaded data. This can be used to date the figures in the report.

# Main dataset
places <- load_places(out_path = PLACE_FILE, use_cache = USE_CACHE)

# Auxiliary tables
types <- load_types(TYPE_FILE, use_cache = USE_CACHE, places = places)
lang_codes <- get_language_codes()

# Edit statistics
edit_data <- load_edits(EDIT_FILE, use_cache = USE_CACHE, places = places)

# Pageview data
pageview_data <- load_pageviews(
  PAGEVIEWS_FILE,
  use_cache = FALSE,
  from = "2023-02-06",
  to = "2024-02-05",
  places = places
)




