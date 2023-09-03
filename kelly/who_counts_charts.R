library(tidyverse)
library(ggcorrplot)
library(directlabels)
library(GGally)



who_counts_format <- theme(
  legend.background = element_blank(),
  axis.ticks = element_line(colour = "white", linewidth = 0.2),
  panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
  panel.grid.minor.x = element_line(colour = "grey70", linewidth = 0.1),
  panel.grid.minor.y = element_blank(),
  panel.background = element_blank(),
  axis.title = element_blank(),
  legend.key = element_blank()
)

gender_colour <- c("mediumpurple", "darkturquoise", "coral2")

##charts




# gender

# View(unique_records)

years <- unique_records %>%
  filter(dob!="2100-01-01") %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(year) %>%
  tally(name="year_tally") %>%
  drop_na() %>%
  ungroup() %>%
  mutate(total=sum(year_tally))


# View(decades)

decades <- unique_records %>%
  filter(dob!="2100-01-01") %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(decade) %>%
  tally(name="decade_tally") %>%
  drop_na()



# year_of_birth_chart <- ggplot(years, aes(year, year_tally)) +
#   geom_line()


decade_of_birth_chart <- ggplot(decades, aes(decade, decade_tally)) +
  geom_col(fill="darkolivegreen")+
  scale_x_continuous(breaks = c(1600, 1700, 1800, 1900, 2000), minor_breaks = c(1650, 1750, 1850, 1950))+
  who_counts_format
  
decade_of_birth_chart


##gender split
years_gender <- unique_records %>%
  filter(dob!="2100-01-01") %>%
  filter(genderLabel!= "male organism") %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(genderLabel, year) %>%
  tally(name="year_tally") %>%
  drop_na()

decades_gender <- unique_records %>%
  filter(dob!="2100-01-01") %>%
  filter(genderLabel!= "male organism") %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(genderLabel, decade) %>%
  tally(name="decade_tally") %>%
  drop_na() %>% 
  mutate(Gender = case_when(genderLabel == "non-binary" | 
                            genderLabel == "trans woman" | 
                            genderLabel == "trans man" | 
                            genderLabel == "genderfluid" |
                              genderLabel == "intersex" |
                              genderLabel == "transgender" 
                            ~ "Transgender, Non-Binary or Intersex",
                            genderLabel=="female" ~ "Female",
                            genderLabel=="male" ~ "Male",
                            TRUE ~ as.character(genderLabel)))
  



# years_gender_chart <- ggplot(years_gender, aes(year, year_tally)) +
#   geom_line(aes(colour=genderLabel))


# decade_gender_chart <- ggplot(decades_gender, aes(decade, decade_tally, colour=Gender)) +
#   geom_line(aes(colour=Gender), size=1)+
#   scale_x_continuous(breaks = c(1600, 1700, 1800, 1900, 2000), minor_breaks = c(1650, 1750, 1850, 1950))+
#   scale_colour_manual(values=gender_colour)+
#   who_counts_format+
#   geom_dl(aes(label=Gender), method=list("first.points", cex=1, vjust=-.7, hjust=-.1))+
#   theme(legend.position = "none")
# 
# decade_gender_chart

decade_gender_chart2 <- ggplot(decades_gender, aes(decade, decade_tally, colour=Gender)) +
  geom_line(aes(colour=Gender), linewidth=1)+
  scale_x_continuous(breaks = c(1600, 1700, 1800, 1900, 2000), minor_breaks = c(1650, 1750, 1850, 1950))+
  scale_colour_manual(values=gender_colour)+
  who_counts_format+
  theme(
    legend.position = c(0.05, .9),
    legend.justification = c("left", "top"),
    legend.direction="vertical",
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
  
  # geom_dl(aes(label=Gender), method=list("first.points", cex=1, vjust=-.5, hjust=-.1 ))

decade_gender_chart2

## indigenous focus
# View(indigenous_records)

indigenous_records <- unique_records %>%
  filter(dob!="2100-01-01") %>%
  filter(genderLabel!= "male organism") %>%
  filter(indigenous == "indigenous") %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(genderLabel, decade) %>%
  tally(name="decade_tally")

decades_indigenous_chart <- ggplot(indigenous_records, aes(decade, decade_tally)) +
  geom_col(fill="darkolivegreen")+
  scale_x_continuous(breaks = c( 1700, 1800, 1900, 2000), minor_breaks = c( 1750, 1850, 1950))+
  who_counts_format

decades_indigenous_chart
##
# 
# source_list <- filter_records %>% 
#   select(source) %>% 
#   distinct() %>% 
#   print(n=51)

filter_records <- filter_records %>% 
  mutate(source2 = case_when(source=="Vic_parliament_aus"~"VIC Parliament",
                             source=="AGSA_creator_aus"~"Gallery SA Creator",
                            source=="aus_citizens_wd"~"Aus Citizen",
                            source=="aus_dictionary_bio"~"Aus Dictionary of Biography",
                            source=="aus_printmakers"~"Aus Printmakers",
                            source=="aus_war_memorial"~"Aus War Memorial",
                            source=="dictionary_of_syd"~"Dictionary of Syd",
                            source=="nat_gallery_vic_artist"~"NGV",
                            source=="national_maritime_museum"~"Nat Maritime Museum",
                            source=="people_of_aus"~"People of Aus",
                            source=="trove_aus"~"Trove",
                            source=="honoursID_aus"~"Aus Honours",
                            source=="obit_of_aus"~"Obituary Aus",
                            source=="VIC_honour_women"~"VIC Honour woman",
                            source=="aus_womens_reg"~"Aus woman reg",
                            source=="sporting_hall_fame_aus"~"Aus sport hall fame",
                            source=="athletics_aus"~"Althetics Aus",
                            source=="aus_olympics"~"Aus Olympics",
                            source=="indigenous_aus"~"Indigenous Aus",
                            source=="aus_poetry_library"~"Aus Poetry",
                            source=="labour_of_aus"~"Labor Aus",
                            source=="VIC_legislative"~"VIC Legislative",
                            source=="parliament_aus"~"Paliament Aus",
                            source=="re_member"~"re_member",
                            source=="place_of_birth_aus"~"Place of Birth Aus",
                            source=="aus_stage"~"Aus Stage",
                            source=="parliament_num_aus"~"Parl Num Aus",
                            source=="Nat_archives_aus"~"Aus Nat Archives",
                            source=="tennis_aus"~"Tennis Aus",
                            source=="SA Govenors"~"SA Govenors",
                            source=="aus_football"~"Aus Football",
                            source=="NSW Govenors"~"NSW Govenors",
                            source=="NSW_legislative"~"NSW Legislative",
                            source=="Tas Govenors"~"TAS Governors",
                            source=="WA Govenors"~"WA Govenors",
                            source=="SA_parliament_aus"~"SA Parliament",
                            source=="west_aus_football"~"WA Football",
                            source=="medical_pioneers"~"Aus Medical Pioneers",
                            source=="footy_facts"~"Footy Facts",
                            source=="Vic Govenors"~"VIC Governors",
                            source=="place_of_death_aus"~"Place of Death Aus",
                            source=="QLD Govenors"~"QLD Governors",
                            source=="rugby_aus"~"Rugby Aus",
                            source=="swim_aus"~"Swim Aus",
                            source=="sa_footbal_hall_fame"~"SA Footy hall of fame",
                            source=="convict_records"~"Convict Records",
                            source=="para_oly_aus"~"Para Olympics Aus",
                            source=="Lord_mayor_Melb"~"Lord Mayor Melb",
                            source=="qld_footbal_hall_fame"~"QLD Footy hall of fame",
                            source=="rowing_aus"~"Rowing Aus",
                            source=="aus_golf"~"Aus Golf",
                            TRUE ~ as.character(source)))

source_matrix <- filter_records %>% 
  select(person, source2) %>%
  mutate(n=1) %>% 
  # group_by(person) %>%
  # add_tally() %>% 
  pivot_wider(names_from = source2, values_from = n) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  select(-person)


corr <- round(cor(source_matrix), 1)

# ggcorrplot(corr, order = 'hclust', outline.col = "white")


who_counts_format2 <- theme(
  legend.background = element_blank(),
  axis.ticks = element_line(colour = "white", linewidth = 0.2),
  panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
  panel.grid.minor.x = element_line(colour = "grey70", linewidth = 0.1),
  panel.grid.minor.y = element_blank(),
  panel.background = element_blank(),
  axis.title = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(size=6),
  legend.title =  element_text(size=8),
)

corr_plot <- ggcorrplot(corr, 
           colors = c("darkcyan", "white", "#E46726"),
           ggtheme = theme_minimal,
           outline.col = "white",
           hc.order = TRUE,
           insig = 'blank',
           tl.cex = 6,
           legend.title = "Correlation"
           )

corr_plot + 
  who_counts_format2
  

# head(corr[, 1:6])



# ggcorrplot(corr, method = "circle", type="lower")


# ggcorrplot(corr, hc.order = TRUE, outline.col = "white", type="lower", lab="true")

# ggcorrplot(corr, hc.order = TRUE, outline.col = "white", type="upper",insig = "blank") 

