##wikidata queries

##creating file of all Australian bios on wikipedia

##notes: need to define what we mean by Australian? Citizen / born in Aus ? lived in Aus etc?
##Rupert Murdoch does not appear in first query (Aus / American), but Lachlan does even w multiple citizenships

library(tidyverse)
library(janitor)
library(WikidataQueryServiceR)

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



##Vic Parliament
Vic_parliament_aus <- query_wikidata(
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


##encycolpeida aus science
# aus_science<- query_wikidata(
#   "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink
#
#   WHERE {
#     SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
#
#     {
#       SELECT DISTINCT ?person WHERE {
#       ?person wdt:P31 wd:Q5 .
#         ?person p:P4228 ?statement0.
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
#   mutate(source="aus_science") %>%
#   mutate(dob=as_date(dob),
#          dod=as_date(dod))


##Aus music centre ID
# aus_music_centre<- query_wikidata(
#   "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink
#
#   WHERE {
#     SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
#
#     {
#       SELECT DISTINCT ?person WHERE {
#       ?person wdt:P31 wd:Q5 .
#         ?person p:P9575 ?statement0.
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
#   mutate(source="aus_music_centre") %>%
#   mutate(dob=as_date(dob),
#          dod=as_date(dod))



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


# aus_lit <- query_wikidata(
#   "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?dob ?dod ?pobLabel ?sitelink
#
#   WHERE {
#     SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
#
#     {
#       SELECT DISTINCT ?person WHERE {
#       ?person wdt:P31 wd:Q5 .
#         ?person p:P8295 ?statement0.
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
#   mutate(source="aus_lit") %>%
#   mutate(dob=as_date(dob),
#          dod=as_date(dod))


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


# View(wp_bio_NSW_gg)

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



all_records <- bind_rows (AGSA_creator_aus, athletics_aus, aus_citizens_wd,
                          aus_dictionary_bio, aus_football, aus_golf,
                          aus_olympics, aus_poetry_library, aus_printmakers,
                          aus_war_memorial, aus_womens_reg, convict_records, dictionary_of_syd, footy_facts,
                          honoursID_aus, indigenous_aus, labour_of_aus, medical_pioneers, Nat_archives_aus,
                          nat_gallery_vic_artist, national_maritime_museum, obit_of_aus, para_oly_aus, parliament_aus,
                          parliament_num_aus, people_of_aus, qld_footbal_hall_fame,re_member, rowing_aus,
                          rugby_aus, sa_footbal_hall_fame, SA_parliament_aus, sporting_hall_fame_aus,
                          swim_aus, tennis_aus, trove_aus, Vic_parliament_aus, west_aus_football, wp_bio_gov_NSW,
                          wp_bio_gov_QLD, wp_bio_gov_SA, wp_bio_gov_VIC, wp_bio_gov_TAS, wp_bio_gov_WA, aus_stage)

View(all_records_wp)

all_records_wp <- all_records %>%
  filter(!is.na(sitelink)) %>%
  group_by(person, source) %>%
  arrange(person, source,dob) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  filter(row_num == 1) %>%
  group_by(person) %>%
  add_tally() %>%
  arrange(desc(n), sitelink, personLabel) %>%
  distinct()

# View(all_na)
#
# all_na <- all_records_wp %>%
#   filter(is.na(dob))

View(clean_records)

clean_records <- all_records_wp %>%
  ungroup() %>%
  select(sitelink, person, personLabel, genderLabel, personDescription, source, dob, dod, pobLabel, n) %>%
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
                           source == "aus_stage" & n==1  & !grepl('Australia', personDescription) &
                             !grepl('aboriginal', personDescription, ignore.case = TRUE)  & !grepl('indigenous', personDescription, ignore.case = TRUE) &
                             !grepl('torres straits', personDescription, ignore.case = TRUE)    ~ "do not keep",
                           grepl('colonial', personDescription) ~ "keep",
                                 TRUE ~ "keep")) %>%
  filter(dob!= as_date("2100-01-01"),
         genderLabel != "male organism") %>% ##filters bloody marvel character out plus phar lap and a dog
  mutate(indigenous = case_when(source == "indigenous_aus"  | grepl('Aboriginal', personDescription,ignore.case = TRUE) |
                                  grepl('indigenous', personDescription, ignore.case = TRUE) |grepl('Torres Strait', personDescription, ignore.case = TRUE)  ~ "indigenous",
                                personLabel =="Deborah Bird Rose" ~ "not indigenous",
                                personLabel =="K. Langloh Parker" ~ "not indigenous",
                                       TRUE ~ "not indigenous"))

# trove_test <- clean_records %>%
#   filter(!grepl('Australia', personDescription) & clean == "keep")
#
# View(trove_test)



# View(filter_records)
filter_records <- clean_records %>%
  filter(clean !="do not keep")


# View(unique_records)

unique_records <- filter_records %>%
      select(sitelink, personLabel, personDescription, genderLabel,n, dob, dod, pobLabel, person, indigenous) %>%
  distinct() %>%
  ungroup() %>%
  group_by(sitelink) %>%
  mutate(count_row = row_number()) %>%
  arrange(sitelink, desc(count_row))


years <- unique_records %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(year) %>%
  tally(name="year_tally") %>%
  drop_na()

decades <- unique_records %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(decade) %>%
  add_tally(name="decade_tally") %>%
  drop_na()



ggplot(years, aes(year, year_tally)) +
  geom_line()


ggplot(decades, aes(decade, decade_tally)) +
  geom_line()


##gender split
years_gender <- unique_records %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(genderLabel, year) %>%
  tally(name="year_tally") %>%
  drop_na()

years_gender_chart <- ggplot(years_gender, aes(year, year_tally)) +
  geom_line(aes(colour=genderLabel))


## indigenous focus
View(indigenous_records)
indigenous_records <- unique_records %>%
  filter(indigenous == "indigenous") %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(genderLabel, decade) %>%
  tally(name="decade_tally")

decades_indigenous_chart <- ggplot(indigenous_records, aes(decade, decade_tally)) +
  geom_col()
