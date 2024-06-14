library(tidyverse)

# Split variable into pipe-delimited string for urlParams
split_batches <- function(variable, batch_size) {
  batches <- split(variable, ceiling(seq_along(variable)/batch_size)) %>%
    map(\(x) paste0(x, collapse = "|")) %>%
    unlist()
  batches
}

# Perform request for vector of pipe-delimited batches
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

# Vectorised claim extraction from Wikidata entities
get_claims <- function(claims, claim_id, value_type) {
  params <- vctrs::vec_recycle_common(list(claims), claim_id, value_type)
  pmap_chr(params, \(claims, id, type) get_one_claim(claims, id, type))
}

# Extract claim from one entity; keep top-ranked claim only
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

# Get 'entity' for a vector of wikidata IDs; the 'entity' contains
# all claims about that wikidata item
get_entities <- function(wikidata_id, .progress = TRUE) {
  batches <- split_batches(wikidata_id, 50)
  url <- paste0(
    "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",
    batches,
    "&props=claims|descriptions|sitelinks|labels&languages=en&sitefilter=enwiki&format=json"
  )
  batched_entities <- map(url, get_batches, .progress = .progress)
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

# Extract basic metadata from wikidata entities
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
