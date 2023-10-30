library(tidyverse)
library(wikkitidy)
source(file.path("scripts", "utils.R"))

# Download all the pages and subcategories under Category:Australian People
get_aussies_category_tree <- function(use_cache, data_dir) {
  save_path <- file.path(data_dir, "wikipedia-category-australian-people.rds")
  if (use_cache) {
    message(glue::glue("Reading data from {save_path}"))
    withr::with_options(list(readr.show_col_types = FALSE), {
      australians <- readr::read_rds(save_path)
    })
  } else {
    # This takes approx 1 hour
    australians <- wikkitidy::build_category_tree("Category:Australian people") %>%
      write_rds(save_path, compress = "bz2")
    message(glue::glue("Data written to {save_path}"))
  }
  australians
}

get_one_wikibase_item <- function(pageid_batch) {
  request <- wiki_action_request() %>%
    query_by_pageid(pageid_batch) %>%
    query_page_properties(
      "pageprops",
      ppprop = "wikibase_item"
    )
  retrieve_all(request)
}

get_wikibase_items <- function(pageid, use_cache, data_dir) {
  save_path <- file.path(data_dir, "wikipedia-to-wikidata.csv")
  if (rlang::is_true(use_cache)) {
    wikibase_items <- readr::read_csv(save_path)
  } else {
    batches <- split_batches(pageid, 50)
    wikibase_items <- batches %>%
      map(get_one_wikibase_item, .progress = "Getting Wikibase IDs") %>%
      bind_rows() %>%
      readr::write_csv(save_path)
  }
  wikibase_items
}

# Data frame of Wikidata properties to query the database
FILTER_PROPERTIES <- tibble::tribble(
  ~source,             ~criteria,
  "aus_citizens_wd",   "?person p:P27 ?statement0.
                        ?statement0 (ps:P27) wd:Q408.",
  "people_of_aus",     "?person p:P9159 ?statement0.",
  "obit_of_aus",       "?person p:P9232 ?statement0.",
  "labour_of_aus",     "?person p:P9245 ?statement0.",
  "indigenous_aus",    "?person p:P9246 ?statement0.",
  "rowing_aus",        "?person p:P11126 ?statement0.",
  "design_and_art_aus","?person p:P1707 ?statement0.",
  "nat_archives_aus",  "?person p:P10856 ?statement0.",
  "rugby_aus",         "?person p:P4799 ?statement0.",
  "athletics_aus",     "?person p:P3915 ?statement0.",
  "tennis_aus",        "?person p:P3698 ?statement0.",
  "sporting_hall_fame_aus", "?person p:P4415 ?statement0.",
  "parliament_num_aus", "?person p:P10020 ?statement0.",
  "swim_aus",          "?person p:P3669 ?statement0.",
  "honoursID_aus",     "?person p:P166 ?award .
                        ?award ps:P166 ?orderaus .
                        ?orderaus wdt:P361 wd:Q1141149 .",
  "parliament_aus",    "?person wdt:P39 wd:Q18912794 .",
  "aus_football",      "?person p:P3546 ?statement0.",
  "para_oly_aus",      "?person p:P10976 ?statement0.",
  "SA_parliament_aus", "?person p:P11128 ?statement0.",
  "aus_war_memorial",  "?person p:P6713 ?statement0.",
  "aus_womens_reg",    "?person p:P4186 ?statement0.",
  "aus_golf",          "?person p:P11191 ?statement0.",
  "convict_records",   "?person p:P9919 ?statement0.",
  "aus_dictionary_bio","?person p:P1907 ?statement0.",
  "aus_olympics",      "?person p:P3682 ?statement0.",
  "medical_pioneers",  "?person p:P9853 ?statement0.",
  "west_aus_football", "?person p:P4571 ?statement0.",
  "aus_poetry_library","?person p:P5465 ?statement0.",
  "VIC_parliament",    "?person p:P8633 ?statement0.",
  "sa_footbal_hall_fame", "?person p:P4623 ?statement0.",
  "qld_footbal_hall_fame", "?person p:P4609 ?statement0.",
  "footy_facts",       "?person p:P4615 ?statement0.",
  "dictionary_of_syd", "?person p:P3794 ?statement0.",
  "NSW_governor",      "?person wdt:P39 wd:Q1528895.",
  "VIC_governor",      "?person wdt:P39 wd:Q1571023",
  "TAS_governor",      "?person wdt:P39 wd:Q1779581",
  "QLD_governor",      "?person wdt:P39 wd:Q1467097",
  "SA_governor",       "?person wdt:P39 wd:Q1840570",
  "WA_governor",       "?person wdt:P39 wd:Q1372518",
  "NSW_legislative_coucil",   "?person wdt:P39 wd:Q1372518.",
  "VIC_honour_women",  "?person p:P166 ?statement0 .
                        ?statement0 (ps:P166) wd:Q7927224.",
  "VIC_legislative_council",   "?person wdt:P39 wd:Q19185341.",
  "Lord_mayor_Melb",   "?person wdt:P39 wd:Q23782667.",
  "place_of_birth_aus", "?person wdt:P19 wd:Q408",
  "place_of_death_aus", "?person wdt:P20 wd:Q408",
  # "AusStage",          "?person p:P8292 ?statement0.", # unreliable
  # "aus_printmakers",   "?person p:P10086 ?statement0.", # unreliable
  # "national_maritime_museum", "?person p:P7769 ?statement0.", # unreliable
  # "nat_gallery_vic_artist", "?person p:P2041 ?statement0.", # unreliable
  # "AGSA_creator_aus",  "?person p:P6804 ?statement0.", # unreliable
  # "trove_aus",         "?person p:P1315 ?statement0.", # unreliable
)

PERSONAL_DATA_PROPS <- tibble::tribble(
  ~label,   ~claim_id, ~value_type,
  "gender", "P21",     "id",
  "dob",    "P569",    "time",
  "dod",    "P570",    "time",
  "pob",    "P19",     "id"
)

query_one <- function(source, criteria, prop_label, prop) {
  query <- glue::glue(
  "SELECT DISTINCT    ?person
   WHERE {{
    SERVICE wikibase:label {{ bd:serviceParam wikibase:language 'en'. }}
    {{
      SELECT DISTINCT ?person WHERE {{
        ?person wdt:P31 wd:Q5 .
        {criteria}
      }}
    }}
   }}")
    result <- WikidataQueryServiceR::query_wikidata(query)
  return(result)
}

get_wikidata_australians <- function(use_cache, data_dir) {
  save_path <- file.path(data_dir, "wikidata-australians.csv")
  if (rlang::is_true(use_cache)) {
    withr::with_options(list(readr.show_col_types = FALSE), {
      message(glue::glue("Reading data from {save_path}"))
      results <- read_csv(save_path)
    })
  } else {
    withr::with_options(list(readr.show_col_types = FALSE), {
      results <- purrr::pmap(FILTER_PROPERTIES, query_one, .progress = "Query Wikidata for Australians")
    })
    names(results) <- FILTER_PROPERTIES$source
    results <- bind_rows(results, .id = "source") %>%
      mutate(person = str_remove(person, fixed("http://www.wikidata.org/entity/"))) %>%
      write_csv(save_path)
    message(glue::glue("Results written to {save_path}"))
  }
  nest(results, source = source, .by = person)
}

get_personal_data <- function(combined_data, use_cache, data_dir) {
  save_path <- file.path(data_dir, "personal-data.csv")
  if (rlang::is_true(use_cache)) {
    message(glue::glue("Reading combined data from {save_path}"))
    personal_data <- read_csv(save_path)
  } else {
    message(glue::glue("Getting additional data from Wikidata. Output will be written to {save_path}"))
    entities <- get_entities(combined_data$person)
    personal_data <- tibble(
      person = map_chr(entities, "id"),
      !!!extract_personal_data(entities),
      !!!extract_metadata(entities),
      is_human = is_human(entities)
    ) %>%
      label_genders_and_places() %>%
      write_csv(save_path)
  }
  personal_data
}

get_quality_indicators <- function(combined_data, use_cache, data_dir, save_freq = 1000) {
  save_path <- file.path(data_dir, "quality-indicators.csv")
  if (rlang::is_true(use_cache)) {
    message(glue::glue("Reading combined data from {save_path}"))
    quality_indicators <- read_csv(save_path)
  } else {
    # This is complicated because the download takes forever, and we want
    # to save intermediate results in case the R sessions is interrupted
    title <- split(combined_data$title, ceiling(seq_along(title)/save_freq))
    message(glue::glue("Quality indicators will be saved to {save_path} every {save_freq} pages"))
    quality_indicators <- imap(
      title,
      \(title, idx) get_quality_indicators_batch(title, idx, save_path),
      .progress = "Downloading quality indicators") %>%
      bind_rows() %>%
      add_column(
        pageid = combined_data$pageid,
        person = combined_data$person,
        .before = 1
      )
  }
}

get_all_extracts <- function(full_dataset, use_cache, data_dir) {
  save_path <- file.path(data_dir, "page-intros.rds")
  if (use_cache) {
    extracts <- read_rds(save_path)
  } else {
    title <- full_dataset %>%
      filter(is_human, on_english_wikipedia) %>%
      pull(title)
    batches <- split_batches(title, 20)
    extracts <- map(batches, get_extract, .progress = "Getting intros")
    extracts <- bind_rows(extracts)
  }
  extracts
}

# Component functions for building top-level routines above
get_extract <- function(title) {
  if (length(title) > 20) {
    rlang::abort("Too many titles. You can only retrieve 20 extracts at a time")
  }
  response <- wiki_action_request() %>%
    query_by_title(title) %>%
    query_page_properties(
      "extracts",
      exintro = TRUE, # just get intro
      explaintext = TRUE, # get plain text rather than html
      exlimit = 20 # you can only get 20 at a time!
    ) %>%
    retrieve_all()
  response
}

get_quality_indicators_batch <- function(title, idx, save_path, resume = FALSE) {
  if (!rlang::is_bare_logical(resume, n = 1)) {
    rlang::abort("`resume` must be either TRUE or FALSE")
  }
  data <- get_xtools_page_info(title, failure_mode = "quiet") %>%
    hoist(assessment, class = "value") %>%
    select(-assessment)
  if (idx == "1") {
    write_csv(data, save_path, append = resume)
  } else {
    write_csv(data, save_path, append = TRUE)
  }
  data
}

get_batches <- function(url) {
  response <- httr2::request(url) %>%
    httr2::req_error(is_error = function(x) FALSE) %>%
    httr2::req_perform()
  if (!httr2::resp_is_error(response)) {
    response_body <- httr2::resp_body_json(response)
  } else {
    response_body <- NA
  }
}

get_one_claim <- function(claims, claim_id, value_type) {
  statements <- pluck(claims, claim_id, .default = NA)
  if (rlang::is_na(statements)) {
    NA
  } else {
    rank <- map_chr(statements, \(stmt) pluck(stmt, "rank", .default = NA))
    rank <- case_when(
      rank == "preferred" ~ 2,
      rank == "normal" ~ 1,
      .default = 0
    )
    preferred_idx <- if (length(rank) > 0) max(rank) else NULL
    value <- pluck(claims, claim_id, preferred_idx, "mainsnak", "datavalue", "value", value_type, .default = NA)
    as.character(value)
  }
}

get_claims <- function(claims, claim_id, value_type) {
  params <- vctrs::vec_recycle_common(list(claims), claim_id, value_type)
  pmap_chr(params, \(claims, id, type) get_one_claim(claims, id, type))
}

PERSONAL_DATA_PROPS <- tibble::tribble(
  ~label,   ~claim_id, ~value_type,
  "gender", "P21",     "id",
  "dob",    "P569",    "time",
  "dod",    "P570",    "time",
  "pob",    "P19",     "id"
)


extract_personal_data <- function(entity) {
  personal_data <- map(entity, extract_one_personal_data) %>%
    bind_rows(.id = "person") %>%
    select(-claim_id, -value_type) %>%
    pivot_wider(names_from = label, values_from = value) %>%
    mutate(
      dob = as_date(dob),
      dod = as_date(dod)
    ) %>%
    select(-person)
}

extract_one_personal_data <- function(entity) {
  claims <- pluck(entity, "claims")
  mutate(
    PERSONAL_DATA_PROPS,
    value = get_claims(claims, claim_id, value_type),
    person = pluck(entity, "id")
  )
}

extract_one_metadata <- function(entity) {
  tibble::tibble_row(
    title = pluck(entity, "sitelinks", "enwiki", "title", .default = NA),
    description = pluck(entity, "descriptions", "en", "value", .default = NA),
    on_english_wikipedia = !is.na(title)
  )
}

extract_metadata <- function(entity) {
  map(entity, extract_one_metadata) %>% bind_rows()
}

is_human <- function(entity) {
  instance_of <- map(entity, list("claims", "P31", 1, "mainsnak", "datavalue", "value", "id"), .default = as.character())
  map_lgl(instance_of, \(prop) "Q5" %in% prop)
}

# Get Wikidata item, extract gender, dob, dod and pob if available
get_entities <- function(wikidata_id) {
  batches <- split_batches(wikidata_id, 50)
  url <- paste0(
    "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",
    batches,
    "&props=claims|descriptions|sitelinks|labels&languages=en&sitefilter=enwiki&format=json"
  )
  batched_entities <- map(url, get_batches, .progress = "Getting personal data")
  entities <- reduce(
    batched_entities,
    \(lhs, rhs) {c(lhs, pluck(rhs, "entities", .default = NA))},
    .init = list()
  )
}

# Get labels of Wikidata entities (e.g. places, genders)
get_labels <- function(wikidata_id, label_name) {
  batches <- split_batches(wikidata_id, 50)
  url <- paste0(
    "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",
    batches,
    "&props=labels&format=json&languages=en"
  )
  batched_entities <- map(url, get_batches, .progress = "Getting labels")
  entities <- reduce(
    batched_entities,
    \(lhs, rhs) {c(lhs, pluck(rhs, "entities", .default = NA))},
    .init = list()
  )
  labels <- map(entities, \(x) rlang::list2(
    wikibase_id = pluck(x, "id"),
    !!label_name := pluck(x, "labels", "en", "value")
  )) %>%
    bind_rows()
  labels
}

label_genders_and_places <- function(personal_data) {
  genders <- unique(personal_data$gender) %>%
    na.omit() %>%
    get_labels("gender_label")

  places <- unique(personal_data$pob) %>%
    na.omit() %>%
    get_labels("place_label")

  personal_data %>%
    left_join(genders, by = join_by(x$gender == y$wikibase_id)) %>%
    mutate(gender = gender_label) %>%
    select(-gender_label) %>%
    left_join(places, by = join_by(x$pob == y$wikibase_id)) %>%
    mutate(pob = place_label) %>%
    select(-place_label)
}


