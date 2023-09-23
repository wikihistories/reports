library(tidyverse)
library(topicmodels)
library(tidytext)
library(janitor)

# View(wikidata)

wikidata <- read_csv("aus_profiles_wikidata_prop.csv") |> 
  clean_names()



# View(wd_text)

wd_text <- wikidata |> 
  # filter(!grepl("Australian rugby league footballer", person_description)) |> 
  select(sitelink, person_description, gender_label) |> 
  filter(!is.na(sitelink)) |> 
  distinct() |> 
  select(person_description, gender_label) |> 
  filter(gender_label!="male organism") |> 
  mutate(gender_label = case_when(gender_label == "non-binary" | 
                                    gender_label == "trans woman" | 
                                    gender_label == "trans man" | 
                                    gender_label == "genderfluid" |
                                    gender_label == "intersex" |
                                    gender_label == "transgender" ~ "Trans NB IS",
                                  gender_label=="female" ~ "Female",
                                  gender_label=="male" ~ "Male",
                            TRUE ~ as.character(gender_label))) 
  
special_stop <- c("australian", "australia", "australia’s", "born")

wd_words <- wd_text |> 
  unnest_tokens(word, person_description) |> 
  anti_join(stop_words) |> 
  filter(!(word %in% special_stop),
         !is.na(word),
         gender_label != "http://www.wikidata.org/.well-known/genid/0de4c5a80b57262088a20354b318e7f9") |> 
  filter(word !="b")



wd_words_count <- wd_words|> 
  # filter(gender_label != "Trans NB IS") |> 
  group_by(gender_label) |> 
  count(word, sort=TRUE)  |>
  mutate(word=reorder(word, n)) |> 
  slice_max(n, n=20) |> 
  arrange((desc(n))) |> 
  ungroup()


word_freq <- ggplot(wd_words_count, aes(n, fct_reorder(word, n), fill=gender_label))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~gender_label, scales="free")+
  labs(x="tf_idf", y=NULL)

word_freq

tf_idf_wd <- wd_words_count |> 
  bind_tf_idf(word, gender_label, n) |> 
  arrange(desc(tf_idf)) 

tf_idf_wd_top <- tf_idf_wd |> 
  group_by(gender_label) |> 
  slice_max(tf_idf, n=12) |> 
  ungroup() |> 
  mutate(word=reorder(word, tf_idf)) 
  

tf_idf_chart <- ggplot(tf_idf_wd_top, aes(tf_idf, word, fill=gender_label))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~gender_label, scales="free")+
  labs(x="tf_idf", y=NULL)
  
tf_idf_chart

ggsave("tf_idf_chart.png", tf_idf_chart, width= 15, height = 15, units=c("cm") )

exclude <- c("footballer", "rules", "rugby", "league")

wd_dtm <- wd_words |> 
  count(gender_label,word) |> 
  filter(!(word %in% exclude)) |>
  cast_dtm(gender_label, word, n)
  
wd_lda <- LDA(wd_dtm, k=4, control = list(seed=2016))
  
test <- wd_lda |> 
  tidy() |> 
  group_by(topic) |> 
  slice_max(beta, n=8) |> 
  ungroup() |> 
  mutate(term = reorder_within(term, beta, topic)) 

wd_lda_chart <- ggplot(test, aes(beta, term, fill=factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales="free")+
  scale_y_reordered()

wd_lda_chart




entertainment <- c("television", "actor", "actress", "singer", "musician", "model")
politics <- c("politics", "politician")
sporting <- c("cricket", "soccer", "coach", "paralympics", "swimmer", "union")
journalism <- c("broadcaster", "journalist")
accademic <- c("accademic", "university", "researcher") 
writer <- c("author", "writer")


special_stop <- c("australian", "australia", "australia’s", "born")

# bi_grams <- wd_text |> 
#   group_by(gender_label) |> 
#   unnest_tokens(bigram, person_description, token = "ngrams", n=2) |> 
#   count(bigram, sort=TRUE)
# 
# bigrams_separated <- bi_grams %>%
#   group_by(gender_label) |> 
#   separate(bigram, c("word1", "word2"), sep = " ")
# 
# bigrams_filtered <- bigrams_separated %>%
#   group_by(gender_label) |> 
#   filter(!word1 %in% stop_words$word) %>%
#   filter(!word2 %in% stop_words$word)
# 
# # new bigram counts:
# bigram_counts <- bigrams_filtered %>% 
#   count(word1, word2, sort=TRUE) |> 
#   arrange(desc(n))
# 
# bigrams_united <- bigrams_filtered |> 
#   group_by(gender_label) |> 
#   unite(bigram, word1, word2, sep=" ")
# 
# 
# bigram_tf_idf <- bigrams_united |> 
#   count(gender_label, bigram) |> 
#   bind_tf_idf(bigram, gender_label, n) |> 
#   arrange(desc(tf_idf)) |> 
#   group_by(gender_label) |> 
#   slice_max(tf_idf, n=12) |> 
#   filter(gender_label != "http://www.wikidata.org/.well-known/genid/0de4c5a80b57262088a20354b318e7f9") 
# 
# View(bigram_tf_idf)
# 
# ggplot(bigram_tf_idf, aes(tf_idf, bigram, fill=factor(gender_label)))+
#   geom_col(show.legend = FALSE)+
#   facet_wrap(~ gender_label, scales="free")+
#   scale_y_reordered()
