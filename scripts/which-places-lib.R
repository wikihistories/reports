# Util functions include functions for getting additional data
# from Wikidata about entities
source(file.path("scripts", "utils.R"))

# All subclasses of 'physical location' in Wikidata
# 'physical location' is declared to be identical to the concept of
# 'place' in many other ontologies, e.g. schema.org
get_place_subclasses <- function() {
  WikidataQueryServiceR::query_wikidata("
    SELECT ?subclass ?subclassLabel
    WHERE {
      ?subclass wdt:P279* wd:Q17334923 .
      SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    }
  ") |>
    dplyr::mutate(
      subclass = stringr::str_remove(subclass, "http://www.wikidata.org/entity/")
    )
}

# Get every wikidata item with country:Australia
# Include 'instance of' property
get_wikidata_places <- function(out_path) {
  message("Getting all subclasses of 'physical location' for filtering ...\n")
  place_types <- get_place_subclasses()

  message("Getting all entities with country:Australia in wikidata ...\n")
  places <- WikidataQueryServiceR::query_wikidata("
    SELECT ?item ?instance_of ?itemLabel
    WHERE {
      ?item wdt:P17 wd:Q408;
            wdt:P31 ?instance_of .
      SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    }
  ") |>
    dplyr::mutate(
      dplyr::across(
        purrr::is_character,
        \(col) stringr::str_remove(col, "http://www.wikidata.org/entity/")
      )
    ) |>
    dplyr::rename(wikidata_id = item) |>
    dplyr::filter(instance_of %in% place_types$subclass)

  print(places)

  distinct_places <- dplyr::distinct(places, wikidata_id)

  message(
    glue::glue("Retrieving metadata for {length(distinct_places)} unique places in the dataset ...\n")
    )

  place_entities <- get_entities(
    distinct_places$wikidata_id,
    "Getting wkd entities for all places"
    )
  place_metadata <- tibble::tibble(
    wikidata_id = distinct_places$wikidata_id,
    !!!extract_metadata(place_entities)
  )

  print(place_metadata)

  message("Combining metadata into original dataframe, and saving ...\n")
  places <- places |>
    dplyr::left_join(place_metadata, by = "wikidata_id") |>
    readr::write_csv(out_path)
  message(glue::glue("Data saved to {out_path}"))
  return(places)
}

load_places <- function(out_path, use_cache = TRUE) {
  if (use_cache) {
    readr::read_csv(out_path)
  } else {
    get_wikidata_places(out_path)
  }
}

# Get information about the types of places in the data set
load_types <- function(out_path, places = NULL, use_cache = TRUE) {
  if (use_cache) {
    readr::read_csv(out_path)
  } else {
    get_all_types(places, out_path)
  }
}
get_all_types <- function(places, out_path) {
  types <- dplyr::distinct(places, instance_of)
  entities <- get_entities(types$instance_of, "Getting wkd entities for all place types")
  types <- tibble::tibble(
    type = map_chr(entities, "id"),
    !!!extract_metadata(entities)
  ) |>
    readr::write_csv(out_path)
  return(types)
}
