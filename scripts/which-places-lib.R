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
# `out_path` should be .rds file
# `places` argument is included for compatibility with
# the loader interface. See `make_loader` below
get_wikidata_places <- function(out_path, places = NULL) {

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

  # Need to use get_entities1, which keeps all sitelinks
  place_entities <- get_entities1(
    distinct_places$wikidata_id,
    "Getting wkd entities for all places"
    )
  place_metadata <- tibble::tibble(
    wikidata_id = distinct_places$wikidata_id,
    !!!extract_metadata(place_entities),
    sitelinks = extract_sitelinks(place_entities)$sitelinks
  )

  print(place_metadata)

  message("Combining metadata into original dataframe, and saving ...\n")
  places <- places |>
    dplyr::left_join(place_metadata, by = "wikidata_id") |>
    readr::write_rds(out_path)
  message(glue::glue("Data saved to {out_path}"))
  return(places)
}

# Get information about the types of places in the data set
load_types <- function(out_path, places = NULL, use_cache = TRUE) {
  if (use_cache) {
    read_file_with_date(out_path)
  } else {
    data <- get_all_types(places, out_path)
    attr(data, "date_created") <- Sys.time()
    return(data)
  }
}

get_all_types <- function(places, out_path) {
  types <- dplyr::distinct(places, instance_of)
  entities <- get_entities(types$instance_of, "Getting wkd entities for all place types")
  types <- tibble::tibble(
    type = map_chr(entities, "id"),
    !!!extract_metadata(entities),
    label = purrr::map_chr(entities, list("labels", "en", "value")),
    sitelinks = extract_sitelinks(entities)$sitelinks,
  ) |>
    readr::write_csv(out_path)
  return(types)
}

# Additional columns

# All sitelinks

extract_sitelinks <- function(entities) {

  sitelinks <- purrr::map(
    entities,
    \(ent) purrr::pluck(ent, "sitelinks", .default = NA) |> names()
    )

  tibble::tibble(
    wikidata_id = names(sitelinks),
    sitelinks = sitelinks
  )

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
get_edits_one_type <- function(titles, type, data_dir) {
  message(glue::glue("Getting {type} edits for {length(titles)} articles ..."))
  edits <- wikkitidy::get_history_count(titles, type = type, failure_mode = "quiet")

  out_path <- file.path(data_dir, glue::glue("{type}.rds"))
  message(glue::glue("Writing output to {out_path}..."))
  tibble::tibble(
    title = titles,
    !!!edits
  ) |>
    readr::write_csv(out_path)

  return(edits)
}

get_edits <- function(out_path, places) {

  subset <- places |>
    filter_english_wikipedia()

  titles <- subset |> dplyr::pull(title)

  data_dir <- dirname(out_path)

  editors <- get_edits_one_type(titles, "editors", data_dir)
  reverts <- get_edits_one_type(titles, "reverted", data_dir)
  bots <- get_edits_one_type(titles, "bot", data_dir)

  edit_data <- subset |>
    dplyr::mutate(
      edits = editors,
      reverts = reverts,
      bots = bots
    ) |>
    readr::write_rds(out_path)

  return(edit_data)
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

# Pageview statistics
# The foundation has made country-level pageview statistics available here:
# https://analytics.wikimedia.org/published/datasets/country_project_page/
# That link contains data from 6 Feb 2023 to the present.
get_granular_pageviews <- function(out_path, from = "2023-02-06", to = "2024-02-06", places) {

  # Where the data is
  base_url <- "https://analytics.wikimedia.org/published/datasets/country_project_page/"

  # Get vector of dates for iteration
  from <- lubridate::as_date(from)
  to <- lubridate::as_date(to)
  dates <- seq(from, to, "1 day") |>
    as.character()

  # The tsvs don't have colnames
  col_headings <- c(
    "country_name",
    "country_code",
    "project",
    "page_id",
    "page_title",
    "wikidata_id",
    "pageviews"
  )

  # Function to iterate
  # Given a date string, retrieve the tsv
  get_one_pageviews <- function(date) {
    url <- glue::glue("{base_url}{date}.tsv")
    data <- readr::read_tsv(
      url,
      col_names = col_headings,
      col_types = readr::cols() # To suppress message
      )
    data <- dplyr::filter(
      data,
      wikidata_id %in% places$wikidata_id
      )
  }

  # Do the business
  withr::local_options(list(readr.show_progress = FALSE)) # Suppress readr messages
  data_frames <- purrr::map(dates, get_one_pageviews, .progress = "Downloading granular pageviews data")
  pageview_data <- dplyr::bind_rows(data_frames)

  # Add metadata
  attr(pageview_data, "from") <- from
  attr(pageview_data, "to") <- to
  readr::write_csv(pageview_data, out_path)

  return(pageview_data)
}


# Data loading
make_loader <- function(getter) {
  loader <- function(out_path, use_cache, places, ...) {
    if (use_cache) {
      read_file_with_date(out_path)
    } else {
      data <- getter(out_path, places, ...)
      attr(data, "date_created") <- Sys.time()
      return(data)
    }
  }
}

load_places <- make_loader(get_wikidata_places)
load_edits <- make_loader(get_edits)
load_types <- make_loader(get_all_types)
load_pageviews <- make_loader(get_granular_pageviews)

