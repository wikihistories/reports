library(tidyverse)
library(WikidataR)

# When I ran this, it only retrieved data on 133k people. There are more than 10 million
# in Wikidata! So we will need to rely on Kelly's numbers to determine which Wikipedia
# articles have P31(Q5)
all_humans <- query_wikidata("
SELECT DISTINCT ?item ?itemLabel
WHERE {
    ?item wdt:P31 wd:Q5;
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en' }
}")

now <- Sys.time() %>% as.integer()

readr::write_csv(all_humans, glue::glue("data/who-counts/all-humans-in-wikidata-{now}.csv"))
