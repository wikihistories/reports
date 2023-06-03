install.packages("tidygraph")

library(tidyverse)
library(ggraph)
library(tidygraph)

detach(package:network, unload=TRUE)

##reading in Michael's WP extraction
data <- readRDS("australian-people-categories-1684481346.rds")


##https://stackoverflow.com/questions/50457926/tidygraph-and-igraph-build-graph-from-dataframe-discrepancy for some reason had to change int to character for tbl_graph to work
 
explore <- data$nodes %>% 
  group_by(type) %>% 
  tally()


node_list <- data$nodes %>% 
  select(-ns) %>% 
  rename(label = title,
         id=pageid) %>% 
  select(-type) %>% 
  drop_na() %>% 
  arrange(id) %>% 
  mutate(id = as.character(id))


edge_list <- data$edge %>% 
  select(-timestamp) %>% 
  rename(from = source,
         to = target) %>% 
  drop_na() %>% 
  arrange(from) %>% 
  mutate_at(vars(to, from), as.character)
  

wp_network <- tbl_graph(
                      nodes =  node_list,
                      edges = edge_list)


ggraph(wp_network, layout = "auto")+
  geom_edge_link()+
  geom_node_point()+
  theme_graph()

# node_check1 <- edge_list %>% 
#   select(from) %>% 
#   rename(check=from)
# node_check2 <- edge_list %>% 
#     select(to) %>% 
#   rename(check=to)
#   
# node_check3 <- node_list %>% 
#   select(id) %>% 
#   rename(check=id)
# 
# 
# nodecheck4 <- rbind(node_check1, node_check2, node_check3) %>% 
#   group_by(check) %>% 
#   tally() %>% 
#   filter(is.na(check))
