

library(tidyverse)


# Reading in FV List-Unlist -----------------------------------------------
load("./Prowess/R Data Prowess/FV List-Unlist.Rdata")

remove(Aud_NSE, Aud_NSE_un)

FV<-FV %>% select(Ownership_group, Co_name, year, Auditor_name)

FV<-FV %>% mutate(Auditor_name = gsub(",","",Auditor_name))

# Creating unique list of Auditors - Nodes --------------------------------

Nodes<-FV %>% 
  distinct(Auditor_name) %>% na.exclude() %>% 
  rowid_to_column() %>% rename(id=rowid)


# Creating unique list of Auditors - Edge list --------------------------------
Edge<-FV %>%  
  group_by(Ownership_group) %>% 
  distinct(Auditor_name) %>% 
  na.exclude() %>% 
  summarise(across(c("Auditor_name"), list(comb = ~ combn(., 2, toString)))) %>% 
  separate(Auditor_name_comb,c("Aud_1", "Aud_2"), sep = "([,])") %>% 
  mutate(Aud_2=str_trim(Aud_2))
  

edge<-Edge %>% ungroup() 


edge<-left_join(edge, Nodes, by=c("Aud_1"="Auditor_name")) %>% 
  rename(from=id) %>% ungroup()

edge<-edge %>% left_join(., Nodes, by=c("Aud_2"="Auditor_name")) %>%  
  rename(to=id)

edge<-edge %>% select(from, to, Ownership_group)



# Network Analysis --------------------------------------------------------


#library(visNetwork)

#visNetwork(Nodes, edge)


library(igraph)

network<-graph_from_data_frame(edge, vertices = Nodes, directed = FALSE)

Degree<-as.data.frame(degree(network, mode = "all", normalized = T)) %>% rownames_to_column("id") %>% 
  rename(Degree_norm = `degree(network, mode = "all", normalized = T)`) %>% 
  mutate(id = as.double(id))

Closeness<-as.data.frame(closeness(network, mode="all", normalized = T)) %>% rownames_to_column("id") %>% 
  rename(Closeness_norm = `closeness(network, mode = "all", normalized = T)`) %>% 
  mutate(id = as.double(id))

Nodes<-Nodes %>% left_join(.,Degree) %>% 
  left_join(., Closeness)


# Saving Image ------------------------------------------------------------
save.image("./Prowess/R Data Prowess/SNA.Rdata")

