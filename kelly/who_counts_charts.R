library(tidyverse)
library(ggcorrplot)
library(directlabels)
library(GGally)
library(ggvenn)
library(readr)
library(scales)



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




##corr plot

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

aus_corr_plot <- corr_plot + 
  who_counts_format2
  

ggsave("aus_corr_plot.png", aus_corr_plot, width= 15, height = 15, units=c("cm") )


##combined data - this is the combined file of files Micheal exacted additional information 
##removing animals and the Port jackson painter
remove <- c(37964165, 232584,2149459)

combined_data <- read_rds("combined_file") %>% 
  mutate(created_at = ymd(created_at),
         dob = ymd(dob),
         dod=ymd(dod)) %>% 
  filter(!(pageid %in% remove),
         !is.na(pageid)) %>% 
  filter(!is.na(genderLabel)) %>% 
  mutate(gender = case_when(genderLabel == "non-binary" | 
                              genderLabel == "trans woman" | 
                              genderLabel == "trans man" | 
                              genderLabel == "genderfluid" |
                              genderLabel == "intersex" |
                              genderLabel == "transgender" 
                            ~ "Transgender, Non-Binary or Intersex",
                            genderLabel=="female" ~ "Female",
                            genderLabel=="male" ~ "Male",
                            TRUE ~ as.character(genderLabel))) %>% 
  mutate(indigenous = case_when(personLabel =="Judith Wright" ~ "not indigenous",
                                TRUE ~ as.character(indigenous)))

View(combined_indigenous)  

combined_indigenous <- combined_data %>% 
  filter(indigenous=="indigenous")

combined_gender_check <- combined_data %>% 
  group_by(gender) %>% 
  tally()

View(combined_gender_check)


##page creation over time

page_creation <- combined_data %>% 
  select(created_at) %>% 
  group_by(created_at) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(cumulative = cumsum(n))



page_creation_cumulative <- ggplot(page_creation, aes(created_at, cumulative))+
  geom_line()+
  geom_area(alpha=.5, fill="darkolivegreen")+
  geom_line(colour="darkolivegreen")+
  theme_minimal()+
  labs(x="Page Creation Date",
       y="Cumulative page count")+
  annotate("text", x = ymd("2001-03-15"), y = 40000, label = "First Australian biography was\nmade for Sherrié Austin on \n26 January 2001", hjust=0, size=4, vjust=1)+
  geom_segment(aes(x=ymd("2001-01-26"), y=0, xend=ymd("2001-01-26"), yend=40000))+
  geom_point(aes(x=ymd("2001-01-26"), y=0),shape=21, fill="white")+
  
  annotate("text", x = ymd("2002-09-19"), y = 20000, label = "First Indigenous Australian\nbiography was made for \nBennelong on 19 July 2002", hjust=0, size=4, vjust=1)+
  geom_segment(aes(x=ymd("2002-07-19"), y=0, xend=ymd("2002-07-19"), yend=20000))+
  geom_point(aes(x=ymd("2002-07-19"), y=0),shape=21, fill="white")
  


page_creation_cumulative

ggsave("page_creation_cumulative.png", page_creation_cumulative,width= 20, height = 10, units=c("cm") )



#page creation by gender

page_creation_gender <- combined_data %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender, created_at) %>% 
  tally() %>% 
  group_by(gender) %>% 
  mutate(cumulative = cumsum(n)) 


page_creation_gender_prop <- combined_data %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender, created_at) %>% 
  tally() %>% 
  group_by(gender) %>% 
  mutate(cumulative = cumsum(n),
         cum_prop = cumulative / sum(n)) 


# View(page_creation_gender)
  
gender_page_creation_facet <- ggplot(page_creation_gender, aes(created_at, cumulative, fill=gender))+
  geom_area()+
  facet_wrap(~gender)+
  scale_fill_manual(values = gender_colour)+
  theme_minimal()+
  labs(x="Page Creation Date",
       y="Cumulative page count",
       fill="Gender")+
  theme(panel.spacing.x = unit(2,"line"))

gender_page_creation_facet

ggsave("gender_page_creation_facet.png", gender_page_creation_facet, width= 20, height = 10, units=c("cm") )


gender_page_creation_line <- ggplot(page_creation_gender, aes(created_at, cumulative, colour=gender))+
  geom_line(linewidth=1)+
  scale_colour_manual(values=gender_colour)+
  theme_minimal()+
  labs(x="Page Creation Date",
       y="Cumulative page count",
       colour="Gender")+
  theme(legend.position = c(0.15, 0.8))

gender_page_creation_line

ggsave("gender_page_creation_line.png", gender_page_creation_line, width= 20, height = 10, units=c("cm") )
  
  

# Quartiles - page creation proportion

quarters <- combined_data %>% 
  ungroup() %>% 
  select(created_at, gender) %>%
  filter(gender=="Male" | gender=="Female") %>% 
  group_by(gender) %>%
  add_tally(name="gender_tally") %>%
  mutate(gender_label=paste0(gender,"\n(",gender_tally,")")) %>%
  group_by(gender) %>%
  reframe(qt = quantile(created_at, c(0.25, 0.5, .75, 1), type=1, na.rm = TRUE))  %>% 
  mutate(quartile = c(0.25, 0.5, .75,1, 0.25, 0.5, .75, 1)) %>% 
  mutate(text = c("25%: Apr 2008 ",
                  "50%: Nov 2013",
                  "75%: Mar 2018",
                  "100%: Jul 2023",
                  
                  "25%: Jul 2007 ",
                  "50%: Aug 2011",
                  "75%: May 2015",
                  "100%: Jul 2023")) 

cum_page_creation_MF <- combined_data %>% 
  filter(gender=="Male" | gender=="Female") %>% 
  group_by(gender, created_at) %>% 
  tally() %>% 
  group_by(gender) %>% 
  mutate(cumulative = cumsum(n),
         cum_prop = cumulative/sum(n)) 

gender_quarter <- ggplot(cum_page_creation_MF, aes(created_at, cum_prop, fill=gender))+
  geom_area(alpha=.7, show.legend = FALSE)+
  facet_wrap(~gender)+
  geom_segment(data=quarters, aes(x=ymd(qt), y=0, xend=qt, yend=quartile+.15))+
  scale_fill_manual(values = gender_colour)+
  theme_minimal()+
  scale_y_continuous(labels = percent, breaks = c(.25, .5, .75, 1))+
  geom_point(data=quarters,aes(x=ymd(qt), y=quartile),shape=21, fill="white")+
  geom_text(data=quarters, aes(x=ymd(qt), y=quartile+0.2, label=text), hjust=1, size=3)+
  labs(x="Page Creation Date",
       y="Proportion of Cumulative pages (%)",
       colour="Gender")

gender_quarter

ggsave("gender_quarter.png", gender_quarter, width= 20, height = 10, units=c("cm") )

page_creation_heat <- combined_data %>% 
  filter(!is.na(dob)) %>% 
  mutate(month_creation = floor_date(created_at, months(6)),
         decade_dob = floor_date(dob, years(5)),
         ) %>% 
  group_by(month_creation,decade_dob) %>% 
  tally() %>% 
  mutate(cumulative = cumsum(n))
  
  # View(page_creation_heat)

heat_map_all <- ggplot(page_creation_heat, aes(month_creation, decade_dob, fill=n))+
  geom_tile()+
  # scale_fill_continuous(high="darkolivegreen", low="cornsilk")
  scale_fill_distiller(palette = "BuPu")+
  theme_minimal()+
  labs(x="Page Creation Date",
       y="Cumulative page count",
       colour="Gender",
       fill="Number of pages")
heat_map_all

ggsave("heat_map_all.png", heat_map_all, width= 20, height = 10, units=c("cm") )

page_creation_heat_gender <- combined_data %>% 
  filter(!is.na(dob)) %>% 
  mutate(month_creation = floor_date(created_at, months(6)),
         decade_dob = floor_date(dob, years(5)),
  ) %>% 
  group_by(gender,month_creation,decade_dob) %>% 
  tally() %>% 
  mutate(cumulative = cumsum(n))

heat_map_gender <- ggplot(page_creation_heat_gender, aes(month_creation, decade_dob, fill=n))+
  geom_tile()+
  # scale_fill_continuous(high="darkolivegreen", low="cornsilk")
  scale_fill_distiller(palette = "BuPu")+
  facet_wrap(~gender)+
  theme_minimal()+
  labs(x="Page Creation Date",
       y="Cumulative page count",
       colour="Gender",
       fill="Number of pages")+
  theme(panel.spacing.x = unit(2,"line"))

heat_map_gender

ggsave("heat_map_gender.png", heat_map_gender, width= 20, height = 10, units=c("cm") )
  



# View(decades)

decades <- combined_data %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(decade) %>%
  tally(name="decade_tally") %>%
  drop_na()

decade_of_birth_chart <- ggplot(decades, aes(decade, decade_tally)) +
  geom_col(fill="darkolivegreen")+
  scale_x_continuous(breaks = c(1600, 1700, 1800, 1900, 2000), minor_breaks = c(1650, 1750, 1850, 1950))+
  theme_minimal()+
  labs(x="Decade of birth",
       y="Number of biographies")

decade_of_birth_chart

ggsave("decade_of_birth_chart.png", decade_of_birth_chart, width= 20, height = 10, units=c("cm") )



##gender split
years_gender <- combined_data %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(gender, year) %>%
  tally(name="year_tally") %>%
  drop_na()

decades_gender <- combined_data %>%
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

indigenous_records_dob <- combined_data %>%
  filter(indigenous == "indigenous") %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(decade) %>%
  tally(name="decade_tally")

decades_indigenous_dob_chart <- ggplot(indigenous_records_dob, aes(decade, decade_tally)) +
  geom_col(fill="darkolivegreen")+
  scale_x_continuous(breaks = c( 1700, 1800, 1900, 2000), minor_breaks = c( 1750, 1850, 1950))+
  who_counts_format

decades_indigenous_dob_chart


indigenous_records_cum <- combined_data %>%
  filter(indigenous == "indigenous") %>%
  mutate(week = week(created_at)) %>%
  # mutate(decade = round(year / 10) *10) %>%
  group_by(week) %>%
  tally(name="tally") %>% 
  mutate(cum_creation = cumsum(tally))

indig_records_cum_chart <- ggplot(indigenous_records_cum, aes(week, cum_creation))+
  geom_line()


##word analysis / token analysis on words OR clustering?

page_creation_3month <- combined_data %>% 
  select(created_at) %>% 
  mutate(month_creation = floor_date(created_at, months(6))) %>% 
  group_by(month_creation) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(cumulative = cumsum(n))

page_creation_3Month_chart <- ggplot(page_creation_3month, aes(month_creation, cumulative))+
  geom_area(alpha=.5, fill="darkolivegreen")+
  geom_line(colour="darkolivegreen")+
  theme_minimal()

page_creation_3Month_chart
years <- combined_data %>%
  mutate(year = year(dob)) %>%
  mutate(decade = round(year / 10) *10) %>%
  group_by(year) %>%
  tally(name="year_tally") %>%
  drop_na() %>%
  ungroup() %>%
  mutate(total=sum(year_tally))
