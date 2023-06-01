library(tidyverse)
library(ggcorrplot)
library(directlabels)



who_counts_format <- theme(
  legend.background = element_rect(fill = "white", size = 4, colour = "white"),
  axis.ticks = element_line(colour = "white", linewidth = 0.2),
  panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
  panel.grid.minor.x = element_line(colour = "grey70", linewidth = 0.1),
  panel.grid.minor.y = element_blank(),
  panel.background = element_blank(),
  axis.title = element_blank()
)

gender_colour <- c("mediumpurple", "darkturquoise", "coral2")

##charts




# gender

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



year_of_birth_chart <- ggplot(years, aes(year, year_tally)) +
  geom_line()


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


decade_gender_chart <- ggplot(decades_gender, aes(decade, decade_tally, colour=Gender)) +
  geom_line(aes(colour=Gender), size=1)+
  scale_x_continuous(breaks = c(1600, 1700, 1800, 1900, 2000), minor_breaks = c(1650, 1750, 1850, 1950))+
  scale_colour_manual(values=gender_colour)+
  who_counts_format+
  geom_dl(aes(label=Gender), method="first.points", cex=.75, hjust=2)

decade_gender_chart

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
  geom_col()
##



source_matrix <- filter_records %>% 
  select(person, source) %>%
  mutate(value=1) %>% 
  pivot_wider(names_from = source, values_from = value) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  select(-person)


corr <- round(cor(source_matrix), 1)

# head(corr[, 1:6])

ggcorrplot(corr)

ggcorrplot(corr, method = "circle", type = "lower", hc.order = TRUE)

ggcorrplot(corr, hc.order = TRUE, outline.col = "white", type="lower")
ggcorrplot(corr, hc.order = TRUE, outline.col = "white", type="lower", lab="true")
ggcorrplot(corr, hc.order = TRUE, outline.col = "white", type="upper",insig = "blank") 

