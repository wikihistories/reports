get_wikidata_places <- function() {
  WikidataQueryServiceR::query_wikidata("
    SELECT ?item
    WHERE {
      ?item wdt:P17 wd:Q408 .
    }
  ") |>
    dplyr::mutate(
      type = dplyr::if_else(stringr::str_detect(item, "entity/Q"), "entity", "property")
    )
}
