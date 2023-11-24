library(tidyverse)
library(lubridate)
library(forcats)
library(janitor)
library(scales)
library(directlabels)
library(tidytext)
library(janeaustenr)

who_counts_format <- theme(
  legend.background = element_blank(),
  axis.ticks = element_line(colour = "white", linewidth = 0.2),
  panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
  panel.grid.minor.x = element_line(colour = "grey70", linewidth = 0.1),
  panel.grid.minor.y = element_blank(),
  panel.background = element_blank(),
  axis.title = element_text(size=10),
  legend.key = element_blank()
)

gender_colour <- c("mediumpurple", "darkturquoise", "coral2")
bi_colour <- c( "gold2", "darkolivegreen4")

View(full_dataset)

data <- full_dataset %>%
  filter(is_human == "TRUE") %>%
  filter(on_english_wikipedia=="TRUE")


data_gender <- data %>%
  filter(!is.na(created_at)) %>%
  filter(!is.na(gender)) %>%
  mutate(gender_group = case_when(gender == "male" ~ "Male",
                                  gender == "female" ~ "Female",
                                    TRUE~ "Trans, Non-Binary or Intersex")) %>%
  arrange(created_at) %>%
  group_by(created_at, gender_group) %>%
  tally() %>%
  group_by(gender_group) %>%
  mutate(cum = cumsum(n)) %>%
  mutate(max = max(cum))


##page creation by gender / cumulative page count

chart_1 <- ggplot(data_gender, aes(created_at, cum))+
  geom_line(aes(colour=gender_group))+
  scale_y_continuous(labels = comma,limits = c(0, 80000), breaks = c(0, 20000, 40000, 60000, 80000 ))+
  theme_minimal()+
  scale_colour_manual(values=gender_colour, name = "Gender")+
  who_counts_format+
  geom_dl(aes(label = max, colour=gender_group), method = list(dl.trans(y=y+0.2), cex=.8, "last.points", vjust=0, hjust=1)) +
  labs(x="Wikipedia Page Creation Date",
       y="Cumulative Wikipedia Page Count",
       caption = "Wikipedia Biographies with gender label and page creation date n=83,000")+
  theme(legend.position = c(0.18, 0.8),
        legend.background = element_rect(colour="white", fill=alpha("white", .6)),
        legend.spacing.y = unit(.03, 'cm'),
        legend.title=element_text(size=10))


chart_1

# ggsave("chart_1.png", chart_1,width= 20, height = 10, units=c("cm") )


##page creation by gender / cumulative page count using log scale

chart_2 <- ggplot(data_gender, aes(created_at, cum))+
  geom_line(aes(colour=gender_group))+
  scale_y_log10(labels = comma, limits = c(1, 120000))+
  theme_minimal()+
  who_counts_format+
  geom_dl(aes(label = max, colour=gender_group), method = list(dl.trans(y=y+0.15), cex=.8, "last.points", vjust=0, hjust=1)) +
  labs(x="Page Creation Date",
       y="Cumulative Wikipedia Page Count (log10 scale)",
       caption = "Wikipedia Biographies with gender label and page creation date n=83,000")+
  scale_colour_manual(values=gender_colour, name = "Gender")+
  theme(legend.position = c(0.18, 0.8),
        legend.background = element_rect(colour="white", fill=alpha("white", .6) ),
        legend.spacing.y = unit(.03, 'cm'),
        legend.title=element_text(size=10))

chart_2

# ggsave("chart_2.png", chart_2, width= 20, height = 10, units=c("cm") )

## article subject DOB (decade calculated via floor, so 1961 and 1969 becomes 1960 - 1969 is NOT rounded to 1970)

data_dob <- data %>%
  mutate(yob = year(dob)) %>%
  filter(!is.na(yob)) %>%
  mutate(floor_date(dob, years(10))) %>%
  group_by(dec_ob) %>%
  tally()

chart_3 <- ggplot(data_dob, aes(dec_ob, n))+
  geom_col(fill="darkolivegreen")+
  scale_y_continuous(labels = comma,limits = c(0, 8000), breaks = c(0, 2000, 4000, 6000, 8000 ))+
  theme_minimal()+
  who_counts_format+
  labs(x="Decade of Birth",
       y="Wikipedia biography Count",
       caption = "Wikipedia Biographies with recorded date of birth on Wikidata n=70,741")

chart_3

# ggsave("chart_3.png", chart_3,width= 20, height = 10, units=c("cm") )

##cumulative page creation

cum_page <- data %>%
  filter(!is.na(created_at)) %>%
  group_by(created_at) %>%
  tally() %>%
  mutate(cum = cumsum(n))

chart_4 <- ggplot(cum_page, aes(created_at, cum))+
  geom_area(fill="darkolivegreen", alpha=.8)+
  theme_minimal()+
  who_counts_format+
  scale_y_continuous(labels = comma,limits = c(0, 85000), breaks = c(0, 20000, 40000, 60000, 80000))+
  labs(x="Wikipedia page creation date",
       y="Cumulative Wikipedia Page Count",
       caption = "Wikipedia Biographies with page creation date n=83,006")


chart_4

# ggsave("chart_4.png", chart_4, width= 20, height = 10, units=c("cm") )


##heat map of creation by date of birth

page_creation_heat <- data %>%
  filter(!is.na(dob)) %>%
  filter(!is.na(created_at)) %>%
  mutate(month_creation = floor_date(created_at, months(6)),
         decade_dob = floor_date(dob, years(10)),
  ) %>%
  group_by(month_creation,decade_dob) %>%
  tally() %>%
  mutate(cumulative = cumsum(n))

chart_5 <- ggplot(page_creation_heat, aes(month_creation, decade_dob, fill=n))+
  geom_tile()+
  scale_fill_distiller(palette = "BuPu")+
  theme_minimal()+
  who_counts_format+
  labs(x="Wikipedia Page Creation Date",
       y="Decade of birth",
       colour="Gender",
       fill="No. of pages",
       caption = "Wikipedia Biographies with page creation date and recorded date of birth on Wikidata n=70,738")+
  theme(legend.spacing.y = unit(.3, 'cm'),
        legend.title=element_text(size=10))


chart_5

# ggsave("chart_5.png", chart_5, width= 20, height = 10, units=c("cm") )

##page health metrics by gender

data_page_health <- data %>%
  filter(!is.na(gender)) %>%
  mutate(gender_group = case_when(gender == "male" ~ "Male \nn=66,370",
                                  gender == "female" ~ "Female \nn=16,558",
                                  TRUE~ "Trans, Non-Binary or Intersex \nn=72")) %>%
  mutate(days_last_edit = ceiling(secs_since_last_edit/86400)) %>%
  select(gender_group, pageviews, revisions, editors, watchers, days_last_edit) %>%
  na.omit() %>%
  pivot_longer(2:6) %>%
  mutate(name = case_when(name == "days_last_edit" ~ "Days since \nlast edit",
                          name == "pageviews" ~ "Pageviews",
                          name == "editors" ~ "Editors",
                          name == "revisions" ~ "Revisions",
                          name == "watchers" ~ "Watchers"
                          )) %>%
  filter(!is.na(value)) %>%
  group_by(gender_group, name) %>%
  mutate(average=round(mean(value),2))

page_health_average <- data_page_health %>%
  select(-value) %>%
  group_by_all() %>%
  summarise()

chart_6 <- ggplot(data_page_health, aes(value, fill = gender_group))+
  geom_density(alpha=.3, aes(fill=gender_group, colour=gender_group), show.legend = FALSE)+
  facet_grid(name~gender_group, switch = "y")+
  scale_x_log10(labels = comma)+
  scale_y_continuous(n.breaks=3, position = "right")+
  scale_fill_manual(values = gender_colour)+
  scale_colour_manual(values = gender_colour)+
  annotate("text",x=9000, y=1.8,label= "mean:", size=3)+
  geom_text(data=page_health_average,aes(x=9000, y=1.3,label= page_health_average$average),size=4)+
  labs(x="Page Metrics (log scale)",
       caption = "Wikipedia Biographies with page statistics and gender label on Wikidata n=83,000")+
  theme_minimal()+
  who_counts_format+
  theme(axis.title.y = element_blank())

chart_6

##text analysis by gender

# View(extracts)
# View(gender_extracts)

extracts_2 <- extracts %>%
  filter(!is.na(pageid))


gender_sum <- gender_extracts %>%
  group_by(gender_group) %>%
  tally()


gender_extracts <- data %>%
  select(pageid, gender) %>%
  left_join(extracts_2, by="pageid") %>%
  filter(!is.na(ns)) %>%
  select(gender, extract) %>%
  mutate(gender_group = case_when(gender == "male" ~ "Male \nn=63,428",
                                  gender == "female" ~ "Female \nn=15,592",
                                  TRUE~ "Trans, Non-Binary or Intersex \nn=69"))


# View(gender_words)

gender_words <- gender_extracts %>%
  unnest_tokens(word, extract) %>%
  filter(!str_detect(word, "\\d"),
          !word %in% c(tidytext::stop_words$word, "australia", "australian", "born")) %>%
  count(gender_group, word, sort = TRUE)


total_words <-  gender_words %>%
  group_by(gender_group) %>%
  summarise(total =sum(n))


gender_words  <- left_join(gender_words, total_words)

gender_words_tf_idf <- gender_words %>%
  # bind_tf_idf(word, gender_group, n) %>%
  mutate(
    total = sum(n),
    tf = n/total,
    .by = gender_group
  ) %>%
  mutate(idf = -log(n() / 4), .by = word) %>%
  mutate(tf_idf = tf / idf, .by = gender_group) %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>%
  group_by(gender_group) %>%
  slice_head(n=20)

gender_words_tf_idf_reorder <- gender_words_tf_idf %>%
  mutate(word = reorder_within(word, n, gender_group))


chart_7 <- ggplot(gender_words_tf_idf_reorder, aes(tf_idf, fct_reorder(word, tf_idf), fill=gender_group))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~gender_group, scales="free")+
  scale_y_reordered()+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_manual(values=gender_colour, name = "Gender")+
  theme_minimal()+
  who_counts_format+
  labs(y=NULL,
       x="Term Frequency - Inverse Document Frequency (TF-IDF)",
       caption = "Wikipedia Biographies with extracted introduction and gender label on Wikidata n=79,026")

chart_7

ggsave("chart_7.png", chart_7, width= 20, height = 10, units=c("cm") )

##Text analysis once all bios with football reference are excluded


View(football)

football <- data %>%
  select(pageid, gender) %>%
  left_join(extracts_2, by="pageid") %>%
  filter(!is.na(ns)) %>%
  select(gender, extract) %>%
  mutate(extract = str_to_lower(extract),
         footy_mention = case_when(str_detect(extract,"football") ~ "Yes",
                                   TRUE ~ "No"
         )) %>%

  mutate(gender_group = case_when(gender == "male" ~ "Male",
                                  gender == "female" ~ "Female",
                                  TRUE~ "Trans, Non-Binary or Intersex")) %>%
  filter(footy_mention == "No")


gender_words_no_footy <- football %>%
  unnest_tokens(word, extract) %>%
  filter(!str_detect(word, "\\d"),
         !word %in% c(tidytext::stop_words$word, "australia", "australian", "born")) %>%
  count(gender_group, word, sort = TRUE)


total_words_no_footy <-  gender_words_no_footy %>%
  group_by(gender_group) %>%
  summarise(total =sum(n))


gender_words_no_footy  <- left_join(gender_words_no_footy, total_words_no_footy)

gender_words_tf_idf_no_footy <- gender_words_no_footy %>%
  # bind_tf_idf(word, gender_group, n) %>%
  mutate(
    total = sum(n),
    tf = n/total,
    .by = gender_group
  ) %>%
  mutate(idf = -log(n() / 4), .by = word) %>%
  mutate(tf_idf = tf / idf, .by = gender_group) %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>%
  group_by(gender_group) %>%
  slice_head(n=20)

gender_words_tf_idf_reorder_no_footy <- gender_words_tf_idf_no_footy %>%
  mutate(word = reorder_within(word, n, gender_group))


chart_8 <- ggplot(gender_words_tf_idf_reorder_no_footy, aes(tf_idf, fct_reorder(word, tf_idf), fill=gender_group))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~gender_group, scales="free")+
  scale_y_reordered()+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_manual(values=gender_colour, name = "Gender")+
  theme_minimal()+
  who_counts_format+
  labs(y=NULL,
       x="Term Frequency - Inverse Document Frequency (TF-IDF)",
       caption = "NO FOOTY mention Wikipedia Biographies with extracted introduction and gender label on Wikidata n=79,026")

chart_8

ggsave("chart_8.png", chart_8, width= 20, height = 10, units=c("cm") )


##football mention in bios total

football_summary <- data %>%
  select(pageid, gender) %>%
  left_join(extracts_2, by="pageid") %>%
  filter(!is.na(ns)) %>%
  select(gender, extract) %>%
  mutate(extract = str_to_lower(extract),
         footy_mention = case_when(str_detect(extract,"football") ~ "Yes",
                                   TRUE ~ "No"
         )) %>%

  mutate(gender_group = case_when(gender == "male" ~ "Male",
                                  gender == "female" ~ "Female",
                                  TRUE~ "Trans, Non-Binary or Intersex")) %>%
  group_by(gender_group, footy_mention) %>%
  tally() %>%
  mutate(prop = n/sum(n)) %>%
  mutate(footy_mention = as_factor(footy_mention),
         footy_mention = fct_rev(footy_mention),
         gender_group = as_factor(gender_group),
         gender_group = fct_relevel(gender_group, c("Male", "Female", "Trans, Non-Binary or Intersex")))



chart_9 <- ggplot(football_summary, aes(gender_group, prop, fill=footy_mention))+
  geom_col(position="fill", width=.8)+
  scale_y_continuous(labels = percent) +
  geom_text(aes(label = percent(round(prop,3))),
          position = position_fill(vjust = 0.5)) +
  theme_minimal()+
  who_counts_format+
  scale_fill_manual(values=bi_colour, name = "Mention of Football")+
  labs(y=NULL,
       x=NULL,
       caption = "Wikipedia Biographies with extracted introduction and gender label on Wikidata n=79,026")+
  theme(legend.position = "top",
        legend.justification = "left",
        legend.background = element_rect(colour="white", fill=alpha("white", .6)),
        legend.spacing.y = unit(.03, 'cm'),
        legend.title=element_text(size=10))

chart_9

ggsave("chart_9.png", chart_9, width= 18, height = 10, units=c("cm") )

##page content assessment by gender

View(data_quality)

data_check <- data_quality %>%
  group_by(gender_group) %>%
  tally()

data_quality <- data %>%
  filter(!is.na(gender)) %>%
  filter(!is.na(class)) %>%
  mutate(gender_group = case_when(gender == "male" ~ "Male\nn=66,370",
                                  gender == "female" ~ "Female\nn=16,558",
                                  TRUE~ "Trans, Non-Binary or Intersex\nn=72")) %>%
  group_by(gender_group, class) %>%
  tally() %>%
  mutate(prop=n/sum(n)) %>%
  mutate(class_new = case_when(class == "FA" ~ "Featured Article",
                               class == "A" ~ "A-Class",
                               class == "GA" ~ "Good Article",
                               class == "B" ~ "B-Class",
                               class == "C" ~ "C-Class",
                               class == "???" ~ "Not Classified",
                               TRUE ~ as.character(class))) %>%
  mutate(class_new = as_factor(class_new),
         class_new = fct_relevel(class_new, c("Featured Article", "A-Class", "Good Article", "B-Class", "C-Class","Start",
                                              "Stub","Not Classified", "Disambig",  "Redirect")),
         class_new = fct_rev(class_new))


chart_10 <- ggplot(data_quality, aes(prop, class_new, fill=gender_group))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label=percent(round(prop, digits = 3))), colour="black", size=3, hjust=-0.1)+
  facet_wrap(~gender_group)+
  theme_minimal()+
  who_counts_format+
  labs(y=NULL,
       x=NULL,
       caption = "Wikipedia Biographies with content assessment category and gender label on Wikidata n=83,000")+
  scale_fill_manual(values=gender_colour, name = "Gender")+
  scale_x_continuous(limits = c(0, .75))+
  theme(panel.grid.minor.x = element_blank())

# mutate(average=round(mean(value),2))

chart_10

ggsave("chart_10.png", chart_10, width= 18, height = 10, units=c("cm") )

##


##page content assessment  split by gender and bios with and without footy mentioned
View(data_quality_no_footy)

data_quality_no_footy <- data %>%
  left_join(extracts_2, by="pageid") %>%
  filter(!is.na(gender)) %>%
  filter(!is.na(class)) %>%
  filter(!is.na(extract)) %>%
  mutate(gender_group = case_when(gender == "male" ~ "Male\nn=66,370",
                                  gender == "female" ~ "Female\nn=16,558",
                                  TRUE~ "Trans, Non-Binary or Intersex\nn=72")) %>%
  mutate(class_new = case_when(class == "FA" ~ "Featured Article",
                               class == "A" ~ "A-Class",
                               class == "GA" ~ "Good Article",
                               class == "B" ~ "B-Class",
                               class == "C" ~ "C-Class",
                               class == "???" ~ "Not Classified",
                               TRUE ~ as.character(class))) %>%
  mutate(class_new = as_factor(class_new),
         class_new = fct_relevel(class_new, c("Featured Article", "A-Class", "Good Article", "B-Class", "C-Class","Start",
                                              "Stub","Not Classified", "Disambig",  "Redirect")),
         class_new = fct_rev(class_new)) %>%
  mutate(extract = str_to_lower(extract),
         footy_mention = case_when(str_detect(extract,"football") ~ "Yes",
                                   TRUE ~ "No"
         )) %>%
  group_by(footy_mention, gender_group, class_new) %>%
  tally() %>%
  mutate(prop=n/sum(n))


chart_11 <- ggplot(data_quality_no_footy, aes(prop, class_new, fill=gender_group))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label=percent(round(prop, digits = 3))), colour="black", size=3, hjust=-0.1)+
  facet_grid(footy_mention~gender_group)+
  theme_minimal()+
  who_counts_format+
  labs(y="Football mentioned in intro para",
       x=NULL,
       caption = "Wikipedia Biographies with content assessment category, intro extract, and gender label on Wikidata n=79,014")+
  scale_fill_manual(values=gender_colour, name = "Gender")+
  scale_x_continuous(limits = c(0, .85))+
  theme(panel.grid.minor.x = element_blank())

chart_11

ggsave("chart_11.png", chart_11, width= 18, height = 10, units=c("cm") )

# page content assessment by footy mentions / no footy mentions

data_quality_no_footy <- data %>%
  left_join(extracts_2, by="pageid") %>%
  filter(!is.na(gender)) %>%
  filter(!is.na(class)) %>%
  filter(!is.na(extract)) %>%
  mutate(gender_group = case_when(gender == "male" ~ "Male\nn=66,370",
                                  gender == "female" ~ "Female\nn=16,558",
                                  TRUE~ "Trans, Non-Binary or Intersex\nn=72")) %>%
  mutate(class_new = case_when(class == "FA" ~ "Featured Article",
                               class == "A" ~ "A-Class",
                               class == "GA" ~ "Good Article",
                               class == "B" ~ "B-Class",
                               class == "C" ~ "C-Class",
                               class == "???" ~ "Not Classified",
                               TRUE ~ as.character(class))) %>%
  mutate(class_new = as_factor(class_new),
         class_new = fct_relevel(class_new, c("Featured Article", "A-Class", "Good Article", "B-Class", "C-Class","Start",
                                              "Stub","Not Classified", "Disambig",  "Redirect")),
         class_new = fct_rev(class_new)) %>%
  mutate(extract = str_to_lower(extract),
         footy_mention = case_when(str_detect(extract,"football") ~ "Football mentioned",
                                   TRUE ~ "Football not mentioned"
         )) %>%
  group_by(footy_mention, class_new) %>%
  tally() %>%
  mutate(prop=n/sum(n))


chart_12 <- ggplot(data_quality_no_footy, aes(prop, class_new))+
  geom_col(show.legend = FALSE, fill="darkolivegreen")+
  geom_text(aes(label=percent(round(prop, digits = 3))), colour="black", size=3, hjust=-0.1)+
  facet_wrap(~footy_mention)+
  theme_minimal()+
  who_counts_format+
  labs(x="Football mentioned in intro para",
       y=NULL,
       caption = "Wikipedia Biographies with content assessment category, intro extract, and gender label on Wikidata n=79,014")+
  scale_fill_manual(values=gender_colour, name = "Gender")+
  scale_x_continuous(limits = c(0, .85))+
  theme(panel.grid.minor.x = element_blank())

chart_12

ggsave("chart_12.png", chart_12, width= 18, height = 10, units=c("cm") )
