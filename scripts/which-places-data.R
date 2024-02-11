# Construct dataset for 2024 annual report
source(file.path("scripts", "which-places-lib.R"))

DATA_DIR <- file.path("data/which-places")

places <- get_wikidata_places()
