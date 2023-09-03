##wikidata queries

##creating file of all "Australians" on wikidata

##notes: what we mean by Australian? Citizen / born in Aus ? lived in Aus etc?


library(tidyverse)
library(janitor)
library(WikidataQueryServiceR)
library(stringr)

# aus_citizens_wd2 <- query_wikidata(
#   "SELECT ?person ?personLabel ?personDescription ?sitelink
# WHERE
# {
#   ?person wdt:P31 wd:Q5 .
#   ?person wdt:P27 wd:Q408.
#   OPTIONAL { ?sitelink schema:about ?person ;
#               schema:inLanguage 'en' ;
#               schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
#   SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'}.
# }")
#
# View(aus_citizens_wd)


##this query picks up anyone who has ever had Aus citizenship inc R Murdoch ;-)

aus_citizens_wd <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    {
      SELECT DISTINCT ?person WHERE {

        ?person p:P27 ?statement0.
        ?statement0 (ps:P27) wd:Q408.


      }
    }

    OPTIONAL{?person wdt:P21 ?gender.}
    OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }") %>%
  mutate(source = "aus_citizens_wd") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))




# View(aus_citizens_wd)


# wp_bio_aus_citizens <- aus_citizens_wd %>%
#   clean_names() %>%
#   filter(!is.na(sitelink)) %>%
#   distinct()


# View(wp_bio_aus_citizens)



##people w People of Australia ID / National Centre of Biography - assuming there will be overalp with Women of Aus list below
people_of_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

  {
      SELECT DISTINCT ?person WHERE {
        ?person p:P9159 ?statement0.

      }
    }


      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="people_of_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


# View(people_of_aus)


##obituries of Aus
obit_of_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P9232 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="obit_of_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


##labour of Aus
labour_of_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P9245 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}


    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="labour_of_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


##Indigenous Aus
# View(indigenous_aus)
indigenous_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P9246 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="indigenous_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


##Rowing Aus
rowing_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P11126 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="rowing_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



##Design and art aus
design_and_art_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P1707 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="Vic_parliament_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


##Nat archives

# View(Nat_archives_aus)
Nat_archives_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P10856 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="Nat_archives_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



##Rugby Aus


rugby_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P4799 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="rugby_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



athletics_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P3915 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="athletics_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


tennis_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P3698 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="tennis_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


# View(AGSA_creator_aus)

## art gallery south australia


AGSA_creator_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P6804 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="AGSA_creator_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


# View(AGSA_creator_aus)



## aus sporting hall of fame

sporting_hall_fame_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P4415 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="sporting_hall_fame_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


## aus parliament MP ID

# View(parliament_aus)
parliament_num_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P10020 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="parliament_num_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))





## aus swimming swimmer ID

swim_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P3669 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="swim_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


# View(honoursID_aus)


honoursID_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
  ?person p:P166 ?award .
  ?award ps:P166 ?orderaus .
  ?orderaus wdt:P361 wd:Q1141149 .

      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"

) %>%
  mutate(source="honoursID_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))

gender_order_wikidata <- honoursID_aus %>%
  filter(!is.na(sitelink)) %>% 
  group_by(personLabel) %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(genderLabel) %>% 
  group_by(genderLabel) %>% 
  tally() %>% 
  mutate(prop = n/sum(n))
  

# View(honoursID_aus)
# View(aus_parliament)

parliament_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
    ?person wdt:P39 wd:Q18912794 .
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="parliament_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))




aus_football <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P3546 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="aus_football") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))

View(aus_football)

para_oly_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P10976 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="para_oly_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



# View(trove_aus)

trove_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P1315 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="trove_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



##members of SA parliament
SA_parliament_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P11128 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="SA_parliament_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



##war memorial

# View(aus_war_memorial)
aus_war_memorial <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P6713 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="aus_war_memorial") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


##aus wmens register
# View(aus_womens_reg)
aus_womens_reg <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P4186 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="aus_womens_reg") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


##aus golf PGA

aus_golf <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P11191 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="aus_golf") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



# View(convict_records)
convict_records<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P9919 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="convict_records") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


# View(aus_dictionary_bio)
aus_dictionary_bio<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P1907 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="aus_dictionary_bio") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



##NGV
# View(nat_gallery_vic_artist)
nat_gallery_vic_artist<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P2041 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="nat_gallery_vic_artist") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



aus_olympics<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P3682 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="aus_olympics") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



##national maritime museum
# View(national_maritime_museum)
national_maritime_museum<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P7769 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="national_maritime_museum") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



medical_pioneers<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P9853 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="medical_pioneers") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


west_aus_football<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P4571 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="west_aus_football") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


aus_poetry_library<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P5465 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="aus_poetry_library") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


aus_printmakers<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P10086 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="aus_printmakers") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


# aus_baseball<- query_wikidata(
#   "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink
#
#   WHERE {
#     SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
#
#     {
#       SELECT DISTINCT ?person WHERE {
#       ?person wdt:P31 wd:Q5 .
#         ?person p:P4476 ?statement0.
#       }
#     }
#
#       OPTIONAL{?person wdt:P21 ?gender.}
#       OPTIONAL{?person wdt:P569 ?dob.}
#     OPTIONAL{?person wdt:P570 ?dod.}
#     OPTIONAL{?person wdt:P19 ?pob.}
#
#     OPTIONAL { ?sitelink schema:about ?person ;
#               schema:inLanguage 'en' ;
#               schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
#   }"
# ) %>%
#   mutate(source="aus_baseball") %>%
#   mutate(dob=as_date(dob),
#          dod=as_date(dod))


##person in the biographical register for the Parliament of Victoria in Australia
re_member<- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P8633 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="re_member") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


sa_footbal_hall_fame <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P4623 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="sa_footbal_hall_fame") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


qld_footbal_hall_fame <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P4609 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="qld_footbal_hall_fame") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



##for an Australian rules football player on the WAFL FootyFacts website
View(footy_facts)

footy_facts <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P4615 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="footy_facts") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))





dictionary_of_syd <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P3794 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="dictionary_of_syd") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))






## state governors

wp_bio_gov_NSW <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1528895.
      }
    }


      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="NSW Govenors") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


# View(wp_bio_gov_NSW)

##governors of VIC

wp_bio_gov_VIC <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1571023
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="Vic Govenors") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


wp_bio_gov_TAS <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1779581
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="Tas Govenors") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


##governors of QLD

wp_bio_gov_QLD <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1467097
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="QLD Govenors") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


##governors of SA

wp_bio_gov_SA <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1840570
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="SA Govenors") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


##governors of WA

# View(wp_bio_gov_WA)
# View(gov_check)
gov_check <- rbind(NSW_legislative, wp_bio_gov_WA) %>%
  group_by(person) %>%
  add_tally() %>%
  filter(n>2)


wp_bio_gov_WA <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1372518
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="WA Govenors") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


View(aus_stage)
aus_stage <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
        ?person p:P8292 ?statement0.
      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
) %>%
  mutate(source="aus_stage") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))

nsw_wa_check <- NSW_legislative %>%



NSW_legislative <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
  ?person p:P39 ?statement0.
  ?statement0 (ps:P39) wd:Q1372518.

      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"

) %>%
  mutate(source="NSW_legislative") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))

VIC_honour_women <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {

  ?person wdt:P31 wd:Q5 .
  ?person p:P166 ?statement0 .
  ?statement0 (ps:P166) wd:Q7927224.

      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"

) %>%
  mutate(source="VIC_honour_women") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


VIC_legislative <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
  ?person p:P39 ?statement0.
  ?statement0 (ps:P39) wd:Q19185341.

      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"

) %>%
  mutate(source="VIC_legislative") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



Lord_mayor_Melb <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink

  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

    {
      SELECT DISTINCT ?person WHERE {
      ?person wdt:P31 wd:Q5 .
  ?person p:P39 ?statement0.
  ?statement0 (ps:P39) wd:Q23782667.

      }
    }

      OPTIONAL{?person wdt:P21 ?gender.}
      OPTIONAL{?person wdt:P569 ?dob.}
    OPTIONAL{?person wdt:P570 ?dod.}
    OPTIONAL{?person wdt:P19 ?pob.}

    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"

) %>%
  mutate(source="Lord_mayor_Melb") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))



# View(place_of_birth_aus)
place_of_birth_aus <- query_wikidata(
  "SELECT DISTINCT    ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink
  WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
  {
    SELECT DISTINCT ?person WHERE {
       ?person wdt:P31 wd:Q5 .
       ?person p:P19 ?statement0.
        ?statement0 (ps:P19) wd:Q408.

    }
  }
  OPTIONAL{?person wdt:P21 ?gender.}
  OPTIONAL{?person wdt:P569 ?dob.}
  OPTIONAL{?person wdt:P570 ?dod.}
  OPTIONAL{?person wdt:P19 ?pob.}



    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
}"

) %>%
  mutate(source="place_of_birth_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))

place_of_death_aus <- query_wikidata(
  "SELECT DISTINCT    ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink
  WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
  {
    SELECT DISTINCT ?person WHERE {
       ?person wdt:P31 wd:Q5 .
       ?person p:P20 ?statement0.
        ?statement0 (ps:P20) wd:Q408.

    }
  }
  OPTIONAL{?person wdt:P21 ?gender.}
  OPTIONAL{?person wdt:P569 ?dob.}
  OPTIONAL{?person wdt:P570 ?dod.}
  OPTIONAL{?person wdt:P19 ?pob.}



    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
}"

) %>%
  mutate(source="place_of_death_aus") %>%
  mutate(dob=as_date(dob),
         dod=as_date(dod))


# View(place_of_death_aus)

all_records <- bind_rows (AGSA_creator_aus, athletics_aus, aus_citizens_wd,
                          aus_dictionary_bio, aus_football, aus_golf,
                          aus_olympics, aus_poetry_library, aus_printmakers,
                          aus_war_memorial, aus_womens_reg, convict_records, dictionary_of_syd, footy_facts,
                          honoursID_aus, indigenous_aus, labour_of_aus, medical_pioneers, Nat_archives_aus,
                          nat_gallery_vic_artist, national_maritime_museum, obit_of_aus, para_oly_aus, parliament_aus,
                          parliament_num_aus, people_of_aus, qld_footbal_hall_fame,re_member, rowing_aus,
                          rugby_aus, sa_footbal_hall_fame, SA_parliament_aus, sporting_hall_fame_aus,
                          swim_aus, tennis_aus, trove_aus, Vic_parliament_aus, west_aus_football, wp_bio_gov_NSW,
                          wp_bio_gov_QLD, wp_bio_gov_SA, wp_bio_gov_VIC, wp_bio_gov_TAS, wp_bio_gov_WA, aus_stage, NSW_legislative,
                          VIC_honour_women,VIC_legislative,Lord_mayor_Melb, place_of_birth_aus, place_of_death_aus)


# View(all_records_wp)

all_records_wp <- all_records %>%
  filter(!is.na(sitelink)) %>%

  group_by(person, source) %>%
  arrange(person, source,pobLabel, dob) %>%
  mutate(dob_row_num = row_number()) %>%
  ungroup() %>%
  filter(dob_row_num == 1) %>% ##this takes the first dob entered in / fixes duplicates if two DOB

  group_by(person, source) %>%
  arrange(person, source,dod) %>%
  mutate(dod_row_num = row_number()) %>%
  ungroup() %>%
  filter(dod_row_num == 1) %>% ##this takes the first dod entered in / fixes duplicates if two DOD

  group_by(person, source) %>%
  arrange(person, source, pobLabel) %>%
  mutate(pob_row_num = row_number()) %>%
  ungroup() %>%
  filter(pob_row_num == 1) %>% ##this takes the first dob entered in / fixes duplicates if two POB
  group_by(person) %>%
  add_tally() %>%
  arrange(desc(n), sitelink, personLabel) %>%
  mutate(indigenous = case_when(source == "indigenous_aus"  | grepl('Aboriginal', personDescription,ignore.case = TRUE) |
                                                                           grepl('indigenous', personDescription, ignore.case = TRUE) |grepl('Torres Strait', personDescription, ignore.case = TRUE)  ~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/MaryAnn_Bin-Sallik'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Long_Jack_Phillipus_Tjakamarra'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Bill_Whiskey_Tjapaltjarri'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Kaylene_Whiskey'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Sally_Scales'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Yinarupa_Nangala'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Patrick_Tjungurrayi'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Nyakul_Dawson'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/John_Gorrie_(elder)'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Deborah_Cheetham_Fraillon'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Mundara_Koorang'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Louisa_Napaljarri'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Bess_Price'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Maggie_Napaljarri_Ross'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Margaret_Kemarre_Turner'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Wenten_Rubuntja'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Warwick_Thornton'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Darryl_White'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Gilbert_McAdam'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/James_Swan_(boxer)'~ "indigenous",

                                                                         sitelink =='https://en.wikipedia.org/wiki/Matthew_Ahmat'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Neville_Perkins'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Steve_Dodd'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Aaron_Pedersen'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Adrian_McAdam'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Chansey_Paech'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Archie_Roach'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Nikita_Ridgeway'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Karla_Dickens'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Stan_Grant_(Wiradjuri_elder)'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/David_Page_(musician)'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Stephen_Page'~ "indigenous",
                                                                         sitelink =='https://en.wikipedia.org/wiki/Owen_Yalandja'~ "indigenous",
                                                                        sitelink =='https://en.wikipedia.org/wiki/David_Cockatoo-Collins'~ "indigenous",
                                sitelink =='https://en.wikipedia.org/wiki/Che_Cockatoo-Collins'~ "indigenous",
                                sitelink =='https://en.wikipedia.org/wiki/Don_Cockatoo-Collins'~ "indigenous",
                                sitelink =='https://en.wikipedia.org/wiki/Nakia_Cockatoo'~ "indigenous",

                                                                          sitelink =='https://en.wikipedia.org/wiki/Whiskey_Tjukangku'~ "indigenous",

                                                                         sitelink =="https://en.wikipedia.org/wiki/Deborah_Bird_Rose" ~ "not indigenous",
                                                                         personLabel =="K. Langloh Parker" ~ "not indigenous",
                                                                         personLabel =="Alfred Edward Gerard" ~ "not indigenous",
                                                                         personLabel =="Charles Dunford Rowley" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/Duncan_McNab" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/Howard_Olney" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/Robert_Donaldson_(politician)" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/William_Garnet_South" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/William_Henry_Gill_(ethnographer)" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/William_Stewart_Harris" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/William_Thomas_(Australian_settler)" ~ "not indigenous",
                                                                         personLabel =="Mark McKenna" ~ "not indigenous",
                                                                         personLabel =="Daisy Bates" ~ "not indigenous",
                                                                         personLabel =="Phyllis Evelyn Duguid" ~ "not indigenous",
                                                                         personLabel =="Charles Duguid" ~ "not indigenous",
                                                                         personLabel =="George Augustus Robinson" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/Francis_McGarry" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/James_Dawson_(activist)" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/Mary_Montgomerie_Bennett" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/Olive_Pink" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/Judith_Wright" ~ "not indigenous",
                                                                         sitelink =="https://en.wikipedia.org/wiki/Janet_Mathews" ~ "not indigenous",
                                                                         sitelink=="https://en.wikipedia.org/wiki/Thomas_King_(novelist)"    ~ "not indigenous",
                                                                         TRUE ~ "not indigenous"))


# View(all_na)
#
# all_na <- all_records_wp %>%
#   filter(is.na(dob))

# View(clean_records)

clean_records <- all_records_wp %>%
  ungroup() %>%
  select(sitelink, person, personLabel, genderLabel, personDescription, source, dob, dod, pobLabel, n,indigenous) %>%
  mutate(clean = case_when(source == "trove_aus" & n<5  & !grepl('Australia', personDescription, ignore.case = TRUE) &
                             !grepl('aboriginal', personDescription, ignore.case = TRUE)  & !grepl('indigenous', personDescription, ignore.case = TRUE) &
                             !grepl('torres straits', personDescription, ignore.case = TRUE)    ~ "do not keep",

                           source == "AGSA_creator_aus" & n<5  & !grepl('Australia', personDescription) &
                             !grepl('aboriginal', personDescription, ignore.case = TRUE)  & !grepl('indigenous', personDescription, ignore.case = TRUE) &
                             !grepl('torres straits', personDescription, ignore.case = TRUE)    ~ "do not keep",

                           source == "aus_printmakers" & n<5  & !grepl('Australia', personDescription) &
                             !grepl('aboriginal', personDescription, ignore.case = TRUE)  & !grepl('indigenous', personDescription, ignore.case = TRUE) &
                             !grepl('torres straits', personDescription, ignore.case = TRUE)    ~ "do not keep",

                           source == "nat_gallery_vic_artist" & n<5  & !grepl('Australia', personDescription) &
                             !grepl('aboriginal', personDescription, ignore.case = TRUE)  & !grepl('indigenous', personDescription, ignore.case = TRUE) &
                             !grepl('torres straits', personDescription, ignore.case = TRUE)    ~ "do not keep",

                           source == "national_maritime_museum" & n<3  & !grepl('Australia', personDescription) &
                             !grepl('aboriginal', personDescription, ignore.case = TRUE)  & !grepl('indigenous', personDescription, ignore.case = TRUE) &
                             !grepl('torres straits', personDescription, ignore.case = TRUE)    ~ "do not keep",

                           source == "aus_music_centre" & n==1  & !grepl('Australia', personDescription) &
                             !grepl('aboriginal', personDescription, ignore.case = TRUE)  & !grepl('indigenous', personDescription, ignore.case = TRUE) &
                             !grepl('torres straits', personDescription, ignore.case = TRUE)    ~ "do not keep",
                           sitelink=="https://en.wikipedia.org/wiki/Thomas_King_(novelist)"    ~ "do not keep",

                           source == "aus_stage" & n<3  & !grepl('Australia', personDescription) &
                             !grepl('aboriginal', personDescription, ignore.case = TRUE)  & !grepl('indigenous', personDescription, ignore.case = TRUE) &
                             !grepl('torres straits', personDescription, ignore.case = TRUE)    ~ "do not keep",
                           grepl('colonial', personDescription) ~ "keep",

                           source == "aus_citizens_wd" ~"keep",
                           source == "place_of_birth_aus" ~"keep",
                           source == "place_of_death_aus" ~"keep",
                           source== "aus_football" ~ "keep",
                           source== "footy_facts" ~ "keep",
                                 TRUE ~ "keep"))

# View(clean_records2)
clean_records2 <- clean_records %>%
  mutate(clean  = case_when(sitelink =="https://en.wikipedia.org/wiki/Argentine_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Brazilian_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Belarusian_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Chinese_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Croatian_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/European_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Hispanic_and_Latin_American_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Korean_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Maltese_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Turkish_Australians" ~ "do not keep",
                            sitelink =="https://en.wikipedia.org/wiki/Vietnamese_Australians" ~ "do not keep",
                            TRUE ~ as.character(clean)
                            ))

# View(filter_records)
filter_records <- clean_records2 %>%
  filter(clean !="do not keep")


write_csv(filter_records, "aus_profiles_wikidata_prop.csv")

# View(unique_records)

unique_records <- filter_records %>%
      select(sitelink, personLabel, personDescription, genderLabel,n, dob, dod, pobLabel, person, indigenous) %>%
  distinct() %>%
  ungroup() %>%
  group_by(sitelink) %>%
  mutate(count_row = row_number()) %>%
  add_tally(name="total_rows") %>%
  arrange(sitelink, desc(count_row)) %>%
  mutate(indig_filter = case_when(indigenous == "indigenous" & count_row ==2 & total_rows >1  ~ "keep",
                                  indigenous == "not indigenous" & count_row ==1 & total_rows >1 ~ "not keep",
                                  indigenous == "not indigenous" & count_row ==1 & total_rows ==1 ~ "keep",
                                  indigenous == "indigenous" & count_row ==1 & total_rows == 1 ~ "keep")) %>%
  filter(indig_filter =="keep") %>%
  select(-n, -count_row, -total_rows, indig_filter)

filter(unique_records, indigenous == "indigenous")


# write_csv(unique_records, "australian_wikidata_records.csv")


#
#


data <- readRDS("australian-people-categories-1684481346.rds")

data_list <- data$nodes
data_edge <- data$edge

View(data_list)
View(data_edge)

##type of record from wikipedia
type <- data_list %>%
  group_by(type) %>%
  tally()

##extracting names from wp url to match with Micheal's extract

View(name_match_wikidata)

name_match_wikidata <- unique_records %>%
  ungroup() %>%
  select(sitelink) %>%
  mutate(name_extract = str_split_i(sitelink, "https://en.wikipedia.org/wiki/", 2) ) %>%
  mutate(name_extract2 = str_replace_all(name_extract, "_", " ")) %>%
  mutate(name_extract3 = str_replace_all(name_extract2, c("%27" =  "'",
                                                          "%C3%B8" = "ø",
                                                          "%C3%96"= "Ö",
                                                          "%C3%A3"= "ã",
                                                          "%C5%8D" = "ō",
                                                          "%C5%8C"= "Ō",
                                                          "%C3%A0"= "à",
                                                          "%C3%A9"= "é",
                                                          "%22"= '"',
                                                          "%C3%B3"= 'ó',
                                                          "%E2%80%93"= '–',
                                                          "%C3%A1"= 'á',
                                                          "%C5%99%C3%AD"= 'ří',
                                                          "%C3%B6"= 'ö',
                                                          "%C3%89"=	'É',
                                                          '%C4%B0'='İ',
                                                          '%C5%9E'='Ş',
                                                           '%C3%AE' ='î',
                                                          '%C3%AA' = 'ê',
                                                          '%C5%BD'='Ž',
                                                          '%C5%BE'='ž',
                                                          '%C4%87'='ć',
                                                          "%C3%93"='Ó',
                                                            '%C3%AB'='ë',
                                                          '%C3%AD' = 'í',
                                                          '%C5%A0' = 'Š',
                                                          '%C5%A1' = 'š',
                                                          '%C3%B0' ='ð',
                                                          '%C3%BC'='ü',
                                                          '%E2%80%9C' = '“',
                                                          '%E2%80%9D' = '” ',
                                                          '%C3%B4'='ô',
                                                          '%C3%A7' ='ç',
                                                           '%C3%A2'= 'â',
                                                          '%C4%8D'='č',
                                                          "%C8%99"='ș',
                                                          '%C3%A4'='ä',
                                                          "%C3%A8" ='è',
                                                          "%26" = '&',
                                                          "%C3%BD"='ý',
                                                          "%CA%BB"="'",
                                                        "%C4%93"='ē',
                                                        "%C5%86"="ņ",
                                                        "%C4%81" = "ā",
                                                        "%C4%9B"="ě",
                                                        "%C4%AB"="ī",
                                                        "%C5%AB"="ū",
                                                        "%C4%9F"="ğ",
                                                        "%C4%B1"="ı",
                                                        "%C3%87"="Ç",
                                                         "%C5%82"= "ł",
                                                        "%C3%AF"="ï",
                                                        '%C3%BA'='ú',
                                                        "%C5%9F"="ş",
                                                        '%C5%81' = 'Ł',
                                                        "%C4%83"='ă',
                                                        "%C4%97" = "ė"))) %>%
  select(name_extract3) %>%
  rename(title=name_extract3) %>%
  mutate(source = "wikidata")


name_match_wikipedia <- data_list %>%
  filter(type =="page") %>%
  select(title) %>%
  mutate(source = "wikipedia")


# View(page)

match <- bind_rows(name_match_wikipedia, name_match_wikidata) %>%
  group_by(title) %>%
  add_tally(name="count") %>%
  mutate(match = case_when(count ==2 ~"match",
                           count ==1 ~ "no match"))

# View(match)

no_match_wikidata <- match %>%
  filter(source == "wikidata" & match=="no match")


View(no_match_wikidata)

match_summary <- match %>%
  group_by(title, match, source) %>%
  summarise() %>%
  group_by(match, source) %>%
  tally()



# View(match_summary)
