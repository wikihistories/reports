# Gets all Australian citizens from Wikidata, since some were missing from
# the original query
library(tidyverse)
library(WikidataR)

data_dir <- file.path("data", "who-counts")

citizens <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    {
      SELECT DISTINCT ?person WHERE {

        ?person p:P27 ?statement0.
        ?statement0 (ps:P27) wd:Q408.


      }
    }

    OPTIONAL{?person wdt:P21 ?gender.}
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }") %>%
  mutate(source = "aus_citizens_wd")

write_csv(citizens, file.path(data_dir, "all-wikidata-australian-citizens.csv"))
