# Construct dataset for 2024 annual report
source(file.path("scripts", "which-places-lib.R"))

USE_CACHE <- TRUE
DATA_DIR <- file.path("data", "which-places")
PLACE_FILE <- file.path(DATA_DIR, "places.csv")
TYPE_FILE <- file.path(DATA_DIR, "place_types.csv")

places <- load_places(out_path = PLACE_FILE, use_cache = USE_CACHE)
types <- load_types(TYPE_FILE, places, use_cache = USE_CACHE)

library(tidyverse)
library(gridExtra)
places <- read_csv("data/which-places/places.csv")
types <- read_csv("data/which-places/place_types.csv")

places_clean <- places %>%
  filter(!is.na(instance_of))

types_clean <- types %>%
  filter(!is.na(type)) %>%
  rename(type_title = title)

top_place_counts <- places_clean %>%
  left_join(types_clean, by = c("instance_of" = "type")) %>%
  filter(!is.na(type_title)) %>%
  count(instance_of, type_title, sort = TRUE) %>%
  slice_max(order_by = n, n = 10, with_ties = TRUE)

plot1 <- ggplot(top_place_counts, aes(x = reorder(type_title, n), y = n, fill = type_title)) + #REMOVE PLOT 1
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), hjust = -0.3) +
  coord_flip() +
  labs(
    title = "Top 10 Most Common Types of Places",
    x = "Type",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


top_place_counts <- places %>%
  count(instance_of, sort = TRUE) %>%
  top_n(10, n) %>%
  left_join(types, by = c("instance_of" = "type"))

plot2 <- ggplot(top_place_counts, aes(x = reorder(title, n), y = n)) + #REMOVE PLOT 2
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Most Common Types of Places",
    x = "Type",
    y = "Count"
  ) +
  theme_minimal()

top_place_counts <- places_clean %>%
  left_join(types_clean, by = c("instance_of" = "type")) %>%
  filter(!is.na(type_title)) %>%
  count(instance_of, type_title, sort = TRUE) %>%
  slice_max(order_by = n, n = 10, with_ties = TRUE)

plot3 <- ggplot(top_place_counts, aes(x = factor(1), y = n, fill = type_title)) +  #REMOVE PLOT 3
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Top 10 Most Common Types of Places",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +

  grid.arrange(plot1, plot2, plot3, ncol = 3) #REMOVE






