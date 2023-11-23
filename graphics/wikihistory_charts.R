library(tidyverse)
library(lubridate)
library(forcats)
library(janitor)
library(scales)
library(directlabels)

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

## articile subject DOB (decade calculated via floor, so 1961 and 1969 becomes 1960 - 1969 is NOT rounded to 1970)



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

##page health




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

