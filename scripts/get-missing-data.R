# Scan through combined-assessed-data and try to get missing gender/dob etc.
# information about each person
library(tidyverse)

DATA_DIR <- file.path("data", "who-counts")

category_australia_biographies <- list.files(DATA_DIR) %>%
  keep(\(x) str_starts(x, "category-australia-biographies")) %>%
  file.path("data", "who-counts", .) %>%
  map(read_csv) %>%
  bind_rows()

split_batches <- function(variable, batch_size) {
  batches <- split(variable, ceiling(seq_along(variable)/batch_size)) %>%
    map(\(x) paste0(x, collapse = "|"))
  batches
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

extract_one_personal_data <- function(entity) {
  claims <- pluck(entity, "claims")
  mutate(
    PERSONAL_DATA_PROPS,
    value = get_claims(claims, claim_id, value_type)
    )
}

extract_personal_data <- function(entity) {
  map(entity, extract_one_personal_data)
}

# Get Wikidata item, extract gender, dob, dod and pob if available
get_entities <- function(wikidata_id) {
  batches <- split_batches(wikidata_id, 50)
  url <- paste0(
    "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",
    batches,
    "&props=claims&format=json"
  )
  batched_entities <- map(url, get_batches, .progress = T)
  entities <- reduce(
    batched_entities,
    \(lhs, rhs) {c(lhs, pluck(rhs, "entities", .default = NA))},
    .init = list()
  )
}

biographies_personal_data <- category_australia_biographies %>%
  mutate(personal_data = get_entities(wikibase_item) %>% extract_personal_data()) %>%
  write_csv(file.path(DATA_DIR, "personal-data-for-biography-pages.csv"))


