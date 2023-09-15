library(tidyverse)


# Download all the pages and subcategories under Category:Australian People
get_aussies_category_tree <- function(use_cache = USE_CACHE, data_dir = DATA_DIR) {
  data_files <- list.files(data_dir)
  aussie_cat_files <- stringr::str_detect(data_files, "australian-people-categories")
  if (!use_cache || sum(aussie_cat_files) == 0) {
    # This takes approx 1 hour
    australians <- wikkitidy::build_category_tree("Category:Australian people")
    readr::write_rds(
      australians,
      file.path(
        data_dir,
        glue::glue("australian-people-categories-{as.integer(Sys.time())}.rds")
      ),
      compress = "gz"
    )
  } else {
    latest_file <- sort(data_files[aussie_cat_files], decreasing = TRUE)[1]
    australians <- readr::read_rds(file = file.path(data_dir, latest_file))
  }
  australians
}

# Data frame of Wikidata properties to query the database
FILTER_PROPERTIES <- tibble::tribble(
  ~source,             ~criteria,
  "aus_citizens_wd",   "?person p:P27 wd:Q408.",
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
  "AGSA_creator_aus",  "?person p:P6804 ?statement0.",
  "sporting_hall_fame_aus", "?person p:P4415 ?statement0.",
  "parliament_num_aus", "?person p:P10020 ?statement0.",
  "swim_aus",          "?person p:P3669 ?statement0.",
  "honoursID_aus",     "?person p:P166 ?award .
                        ?award ps:P166 ?orderaus .
                        ?orderaus wdt:P361 wd:Q1141149 .",
  "parliament_aus",    "?person wdt:P39 wd:Q18912794 .",
  "aus_football",      "?person p:P3546 ?statement0.",
  "para_oly_aus",      "?person p:P10976 ?statement0.",
  "trove_aus",         "?person p:P1315 ?statement0.",
  "SA_parliament_aus", "?person p:P11128 ?statement0.",
  "aus_war_memorial",  "?person p:P6713 ?statement0.",
  "aus_womens_reg",    "?person p:P4186 ?statement0.",
  "aus_golf",          "?person p:P11191 ?statement0.",
  "convict_records",   "?person p:P9919 ?statement0.",
  "aus_dictionary_bio","?person p:P1907 ?statement0.",
  "nat_gallery_vic_artist", "?person p:P2041 ?statement0.",
  "aus_olympics",      "?person p:P3682 ?statement0.",
  "national_maritime_museum", "?person p:P7769 ?statement0.",
  "medical_pioneers",  "?person p:P9853 ?statement0.",
  "west_aus_football", "?person p:P4571 ?statement0.",
  "aus_poetry_library","?person p:P5465 ?statement0.",
  "aus_printmakers",   "?person p:P10086 ?statement0.",
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
  "AusStage",          "?person p:P8292 ?statement0.",
  "NSW_legislative_coucil",   "?person wdt:P39 wd:Q1372518.",
  "VIC_honour_women",  "?person ps:P166 wd:Q7927224.",
  "VIC_legislative_council",   "?person wdt:P39 wd:Q19185341.",
  "Lord_mayor_Melb",   "?person wdt:P39 wd:Q23782667.",
  "place_of_birth_aus", "?person wdt:P19 wd:Q408",
  "place_of_death_aus", "?person wdt:P20 wd:Q408"
)

# Querying for each property seperately ensures that the query
# doesn't breach the 300ms limit
DATA_PROPERTIES <- tibble::tribble(
  ~prop_label,   ~prop,
  "genderLabel", "OPTIONAL{?person wdt:P21 ?gender.}",
  "dob",         "OPTIONAL{?person wdt:P569 ?dob.}",
  "dod",         "OPTIONAL{?person wdt:P570 ?dod.}",
  "pobLabel",    "OPTIONAL{?person wdt:P19 ?pob.}"
)

query_one <- function(source, criteria, prop_label, prop) {
  query <- glue::glue(
  "SELECT DISTINCT    ?person ?personLabel ?personDescription ?{prop_label} ?sitelink
   WHERE {{
    SERVICE wikibase:label {{ bd:serviceParam wikibase:language 'en'. }}
    {{
      SELECT DISTINCT ?person WHERE {{
        ?person wdt:P31 wd:Q5 .
        {criteria}
      }}
    }}

    {prop}

    OPTIONAL {{ ?sitelink schema:about ?person ;
     schema:inLanguage 'en' ;
     schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ]
    }}.

   }}")
    result <- WikidataQueryServiceR::query_wikidata(query)
  return(result)
}

get_wikidata_australians <- function(use_cache = USE_CACHE, data_dir = DATA_DIR) {
  if (rlang::is_true(use_cache)) {
    readr::read_rds(file.path(data_dir, "wikidata-australians.rds"))
  } else {
    withr::with_options(list(readr.show_col_types = FALSE), {
      query_params <- cross_join(FILTER_PROPERTIES, DATA_PROPERTIES)
      results <- purrr::pmap(query_params, query_one, .progress = "Query Wikidata for Australians")
    })
    tibble::tibble(query_params, results)
  }
}

clean_wikidata_australians <- function(query_result) {
  query_result %>%
    filter(purrr::map_int(wikidata_australians$results, nrow) > 0) %>%
    mutate(results = purrr::map(results, clean_dates)) %>%
    tidyr::unnest()
}

clean_dates <- function(result_tbl) {
  mutate(
    result_tbl,
    across(where(ends_with("ob"))),
    \(x) as_datetime(x))
}
