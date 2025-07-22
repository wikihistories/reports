# Util functions include functions for getting additional data
# from Wikidata about entities
source(file.path("scripts", "utils.R"))

# What kinds of events are known in Wikidata?
get_occurrence_subclasses <- function() {
  WikidataQueryServiceR::query_wikidata("
    SELECT ?subclass ?subclassLabel
    WHERE {
      # All recursive subclasses of occurrence
      {
         ?subclass wdt:P279* wd:Q1190554 .
      }
      SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    }
  ") |>
    dplyr::mutate(
      subclass = stringr::str_remove(subclass, "http://www.wikidata.org/entity/")
    )
}

# All 'Australian' events in Wikidata
get_wikidata_occurrences <- function(out_path, occurrences = NULL) {

  message("Getting all subtypes of occurrence (Q1190554) ...\n")
  occurrence_types <- get_occurrence_subclasses()

  message("Getting all entities with country:Australia in wikidata ...\n")
  occurrences <- WikidataQueryServiceR::query_wikidata("
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
    dplyr::filter(instance_of %in% occurrence_types$subclass)

  print(occurrences)

  distinct_occurrences <- dplyr::distinct(occurrences, wikidata_id)

  message(
    glue::glue("Retrieving metadata for {nrow(distinct_occurrences)} unique occurrences in the dataset ...\n")
  )

  # Need to use get_entities1, which keeps all sitelinks
  occurrence_entities <- get_entities1(
    distinct_occurrences$wikidata_id,
    "Getting wkd entities for all occurrences"
  )
  occurrence_metadata <- tibble::tibble(
    wikidata_id = distinct_occurrences$wikidata_id,
    !!!extract_metadata(occurrence_entities),
    sitelinks = extract_sitelinks(occurrence_entities)$sitelinks,
    !!!extract_spacetime(occurrence_entities)
  )

  print(occurrence_metadata)

  message("Combining metadata into original dataframe, and saving ...\n")
  occurrences <- occurrences |>
    dplyr::left_join(occurrence_metadata, by = "wikidata_id") |>
    readr::write_rds(out_path)
  message(glue::glue("Data saved to {out_path}"))
  return(occurrences)
}

# Get event times and locations
extract_spacetime <- function(entities) {

  latitude <- purrr::map_chr(entities, \(ent) purrr::pluck(ent, "claims") |> get_one_claim("P625", "latitude"))
  longitude <- purrr::map_chr(entities, \(ent) purrr::pluck(ent, "claims") |> get_one_claim("P625", "longitude"))
  start_time <- purrr::map_chr(entities, \(ent) purrr::pluck(ent, "claims") |> get_one_claim("P580", "time"))
  end_time <- purrr::map_chr(entities, \(ent) purrr::pluck(ent, "claims") |> get_one_claim("P582", "time"))

  tibble::tibble(latitude, longitude, start_time, end_time)

}


# Wrap functions
make_loader <- function(getter) {
  loader <- function(out_path, use_cache, occurrences, ...) {
    if (use_cache) {
      read_file_with_date(out_path)
    } else {
      data <- getter(out_path=out_path, occurrences=occurrences, ...)
      attr(data, "date_created") <- Sys.time()
      return(data)
    }
  }
}

load_occurrences <- make_loader(get_wikidata_occurrences)
