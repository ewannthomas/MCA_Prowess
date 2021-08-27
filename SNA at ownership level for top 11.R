


library(tidyverse)


load("./Prowess/R Data Prowess/FV List-Unlist.Rdata")

remove(Aud_NSE, Aud_NSE_un)


FV<-FV %>% filter(Indust_type==1) %>% 
  filter(!Ownership_group %in% c("Central Govt. - Commercial Enterprises", "State Govt. - Commercial Enterprises", 
                                 "State Bank of India Group", "State and Private sector","Private (Indian)","Private (Foreign)"))

FV<-FV%>% mutate(
  Mkt_capital = case_when(
    Mkt_capital==0 ~ NA_real_,
    TRUE ~ Mkt_capital)) %>% select(Ownership_group, Co_name, year, Auditor_name,Mkt_capital) %>% 
  group_by(Ownership_group, Auditor_name) %>%  #creating Audit tenure
  mutate(Aud_tenure= case_when(
    !is.na(Auditor_name) ~ max(row_number()), 
    TRUE ~ NA_integer_)) %>% ungroup()

FV<-FV %>% group_by(Ownership_group) %>% mutate(sum=sum(Mkt_capital,na.rm=T)) %>% 
  filter(sum>=31392320) %>% ungroup




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


Aud_nodes_top<-bind_rows(Aud_nodes_1, Aud_nodes_2, Aud_nodes_3) %>% 
  rowid_to_column() %>% rename(id=rowid) %>%  
  mutate(group = case_when(
    id<=177 ~ "Auditors",
    id>=178 & id<=180 ~ "Business Groups",
    TRUE ~ "Companies"))

rm(Aud_nodes_1, Aud_nodes_2, Aud_nodes_3)

# Creating unique list of Auditors - Aud_edge list --------------------------------

Aud_edge<-FV %>% distinct(Ownership_group, Co_name) %>% na.exclude() %>% 
  rename(group1 = Ownership_group,
         group2 = Co_name) %>% 
  bind_rows(FV %>% distinct(Co_name, Auditor_name, Aud_tenure) %>% na.exclude() %>% 
              rename(group1 = Co_name,
                     group2 = Auditor_name))

edge<-left_join(Aud_edge, Aud_nodes_top, by=c("group1"="label")) %>%  
  rename(from=id) %>% ungroup() %>% select(-group)

edge<-edge %>% left_join(., Aud_nodes_top, by=c("group2"="label")) %>%  
  rename(to=id) %>% select(-group)

edge<-edge %>% select(from, to, Aud_tenure) %>% rename(weight=Aud_tenure)



# Network Analysis --------------------------------------------------------


library(visNetwork)

visNetwork(Aud_nodes_top, edge, height = "1500px", width = "100%") %>% 
  visOptions(highlightNearest = list(enabled=TRUE, degree=1, hover=TRUE),
             selectedBy = "group",
             nodesIdSelection = TRUE) %>% 
  visGroups(groupname = "Business Groups", color="red", shape="triangle") %>%
  visGroups(groupname = "Companies", color="violet", shape="square") %>%
  visGroups(groupname = "Auditors") %>% 
  visEdges(arrows = "to") %>% 
  visLayout(randomSeed = 123)








