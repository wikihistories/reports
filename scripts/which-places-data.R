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
TEXTS_FILE <- file.path(DATA_DIR, "enwiki_texts.csv.bz2")
LANG_FILE <- file.path(DATA_DIR, "lang-codes.csv")

# NB: the "load_" functions below all add a metadata attribute, "time_created"
# to the loaded data. This can be used to date the figures in the report.

# Main dataset
places <- load_places(out_path = PLACE_FILE, use_cache = USE_CACHE)

# Auxiliary tables
types <- load_types(out_path = TYPE_FILE, use_cache = USE_CACHE, places = places)
lang_codes <- load_lang_codes(out_path = LANG_FILE, use_cache = USE_CACHE)

# Edit statistics and text; these can be joined by 'title'
edit_data <- load_edits(EDIT_FILE, use_cache = USE_CACHE, places = places)
text_data <- load_texts(TEXTS_FILE, use_cache = USE_CACHE, places = places)

#TODO: compress enwiki texts; might be necessary to change 'loader' code.

# Pageview data
PGV_FROM <- "2023-02-06"
PGV_TO <- "2024-02-05"
pageview_data <- load_pageviews(
  PAGEVIEWS_FILE,
  use_cache = USE_CACHE,
  from = PGV_FROM,
  to = PGV_TO,
  places = places
)
if (USE_CACHE) {
  attr(pageview_data, "from") <- lubridate::as_date(PGV_FROM)
  attr(pageview_data, "to") <- lubridate::as_date(PGV_TO)
}
