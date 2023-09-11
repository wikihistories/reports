library(tidyverse)
library(readr)

list_of_files <- list.files(recursive = TRUE,
                            pattern = "combined-assessed-data-rows",
                            full.names = TRUE)

combined <- readr::read_csv(list_of_files, id = "file_name")

View(combined)

combined2 <- combined %>% 
  filter(!is.na(sitelink)) %>% 
  select(-file_name)
  
combined_file <- tempfile()


write_rds(combined2, combined_file)

read_rds(combined_file)

write_rds(combined2, "/Users/kellytall/Documents/github/UTS_ARC/wikihistories/reports/kelly/combined_file")
