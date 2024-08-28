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
    glue::glue("Retrieving metadata for {nrow(distinct_places)} unique places in the dataset ...\n")
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
    !!!extract_metadata(entities),
    sitelinks = extract_sitelinks(entities)$sitelinks,
  ) |>
    readr::write_csv(out_path)
  return(types)
}

# Additional columns

# All sitelinks

extract_sitelinks <- function(entities) {

  extract_one_sitelink <- function(entity) {
    row_data <- list(
      sitelinks = purrr::pluck(entity, "sitelinks") |> names() |> list(),
      wikidata_id = purrr::pluck(entity, "title")
    )
    tibble::as_tibble_row(row_data)
  }

  purrr::map(entities, extract_one_sitelink) |>
    dplyr::bind_rows() |>
    tidyr::unnest(sitelinks) |>
    dplyr::mutate(
      code = stringr::str_remove(sitelinks, "wiki")
    ) |>
    tidyr::nest(sitelinks = c(sitelinks, code))

}

get_language_codes <- function() {
  # TODO: Add support for meta queries to wikkitidy and/or default behaviour
  resp <- httr2::request("https://en.wikipedia.org/w/api.php?action=query&format=json&formatversion=2&meta=siteinfo&siprop=languages") |>
    httr2::req_perform()
  data <- httr2::resp_body_json(resp) |>
    purrr::pluck("query", "languages") |>
    dplyr::bind_rows()
  return(data)
}

filter_english_wikipedia <- function(places) {
  places |>
    dplyr::filter(on_english_wikipedia) |>
    dplyr::distinct(title, .keep_all = TRUE)
}

# Edit data: (1) number of edits; (2) proportion of reverts
get_edits <- function(places) {
  edit_data <- places |>
    filter_english_wikipedia() |>
    dplyr::mutate(
      total_edits = wikkitidy::get_history_count(title, type = "editors"),
      total_reverts = wikkitidy::get_history_count(title, type = "reverted")
      ) |>
    tidyr::unnest(
      cols = c(total_edits, total_reverts),
      names_sep = "_"
    ) |>
    dplyr::select(
      wikidata_id, dplyr::starts_with("total")
    )

  dplyr::left_join(
    places, edit_data,
    by = "wikidata_id"
  )
}

# Readership data

# Native names
# This is causing a bit of a problem. If you list the templates on the page,
# the 'native name' embedded in the info box is not included.
# It may be necessary to retrive the text for *each page* and then use a regular
# expression to find either "native_name" or "Native name".
# An alternative would simply be to use the native name data on
# Wikidata, but this doesn't match Wikipedia (e.g. Brisbane's native name
# isn't there on Wikidata...)
get_native_names <- function(places) {
  titles <- places |>
    filter_english_wikipedia() |>
    dplyr::pull(title)

  wikkitidy::wiki_action_request() |>
    wikkitidy::query_by_title(titles) |>
    wikkitidy::query_page_properties(
      "templates",
      # tltemplates = "Template:Native name"
    ) |>
    wikkitidy::retrieve_all()
}
