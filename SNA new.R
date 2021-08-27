



library(tidyverse)


# Reading in FV List-Unlist -----------------------------------------------
load("./Prowess/R Data Prowess/FV List-Unlist.Rdata")

remove(Aud_NSE, Aud_NSE_un)

FV<-FV %>% select(Ownership_group, Co_name, year, Auditor_name) %>% 
  group_by(Ownership_group, Co_name, Auditor_name) %>%  #creating Audit tenure
  mutate(Aud_tenure= case_when(
    !is.na(Auditor_name) ~ max(row_number()), 
    TRUE ~ NA_integer_)) %>% ungroup()




# Creating unique list of Auditors and Ownership Groups Nodes --------------------------------

Aud_nodes_1<-FV %>% 
  distinct(Auditor_name) %>% na.exclude() %>% 
  rename(label = Auditor_name)

Aud_nodes_2<-FV %>% 
  distinct(Ownership_group) %>% na.exclude() %>% 
  rename(label = Ownership_group)

Aud_nodes_3<-FV %>% 
  distinct(Co_name) %>% na.exclude() %>% 
  rename(label = Co_name)

Aud_nodes<-bind_rows(Aud_nodes_1, Aud_nodes_2, Aud_nodes_3) %>% 
  rowid_to_column() %>% rename(id=rowid)

rm(Aud_nodes_1, Aud_nodes_2, Aud_nodes_3)

# Creating unique list of Auditors - Aud_edge list --------------------------------
Aud_edge<-FV %>% distinct(Ownership_group, Co_name) %>% na.exclude() %>% 
  rename(group1 = Ownership_group,
         group2 = Co_name) %>% 
  bind_rows(FV %>% distinct(Co_name, Auditor_name, Aud_tenure) %>% na.exclude() %>% 
              rename(group1 = Co_name,
                     group2 = Auditor_name))

edge<-left_join(Aud_edge, Aud_nodes, by=c("group1"="label")) %>%  
  rename(from=id) %>% ungroup()

edge<-edge %>% left_join(., Aud_nodes, by=c("group2"="label")) %>%  
  rename(to=id)

edge<-edge %>% select(from, to, Aud_tenure) %>% rename(weight=Aud_tenure)


# Network Analysis --------------------------------------------------------


library(igraph)

network<-graph_from_data_frame(edge, vertices = Aud_nodes, directed = TRUE)

Degree<-as.data.frame(degree(network, mode = "in", normalized = T)) %>% rownames_to_column("id") %>% 
  rename(Degree_norm = `degree(network, mode = "in", normalized = T)`) %>% 
  mutate(id = as.double(id))

Closeness<-as.data.frame(closeness(network, mode="in", normalized = T)) %>% 
  rownames_to_column("id") %>% 
  rename(Closeness_norm = `closeness(network, mode = "in", normalized = T)`) %>% 
  mutate(id = as.double(id))

Betweeness<-as.data.frame(betweenness(network, directed = TRUE)) %>% 
  rownames_to_column("id") %>% 
  rename(Betweeness = `betweenness(network, directed = TRUE)`) %>% 
  mutate(id = as.double(id))

Aud_nodes<-Aud_nodes %>% left_join(.,Degree) %>% 
  left_join(., Closeness) %>% 
  left_join(., Betweeness)


# Saving Image ------------------------------------------------------------
save.image("./Prowess/R Data Prowess/SNA.Rdata")

