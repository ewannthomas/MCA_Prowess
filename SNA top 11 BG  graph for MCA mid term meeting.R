


library(tidyverse)
library(igraph)
library(visNetwork)


# Reading in FV List-Unlist -----------------------------------------------
load("./Prowess/R Data Prowess/FV List-Unlist.Rdata")

remove(Aud_NSE, Aud_NSE_un)

load("./top_50.Rdata")

FV<-FV %>% select(Ownership_group, Co_name, year, Auditor_name, Groupstand) %>% 
  mutate(Top_10 = case_when(
    Groupstand==1 & Ownership_group %in% c("Tata Group","Reliance Group [Mukesh Ambani]","I.T.C. (F) Group",
                                           "Vedanta Group", "Birla Aditya Group","Uni Lever (F) Group",
                                           "Bharti Telecom Group","WIPRO Group","Larsen & Toubro Group",
                                           "Mahindra & Mahindra Group","Sun Pharmaceutical Group") ~ 1,
    Groupstand==0 ~ NA_real_,
    TRUE ~ 0),
    Top_50 = case_when(
      
      Groupstand==1 & Ownership_group %in% top_50 ~ 1,
      Groupstand==0 ~ NA_real_,
      TRUE ~ 0),
    Top_10_standalone = case_when(
      Groupstand==1 & Top_10==1 ~ 1,
      Groupstand==0 ~ 0
    ))



# Centrality by Ownership Group -----------------------------------------





fluk<-FV %>% filter(Top_10==1) %>% group_by(Ownership_group) %>% nest() %>% na.exclude()




flah<-function(x){
  
  Edge<-x %>%  
    group_by(Co_name) %>% 
    distinct(Auditor_name) %>% 
    na.exclude()
  
  edge<-graph_from_data_frame(Edge)
  
  V(edge)$type<-bipartite.mapping(edge)$type
  
  edge<-bipartite.projection(edge, multiplicity = TRUE, which = "true")

}


klah<-function(x){
  nodes<-x %>% distinct(Auditor_name) %>% na.exclude() %>% 
    rename(label = Auditor_name) %>% 
    rowid_to_column() %>% rename(id=rowid)
}


blah<-function(x,y){
  edges<-as_data_frame(x) %>%
    rename(source = from,
           dest = to) %>% 
    left_join(.,y, by=c("source"="label")) %>% 
    rename(from=id) %>%
    left_join(.,y, by=c("dest"="label")) %>%
    rename(to=id) %>% select(from, to, weight)
}




close<-function(x){
  closeness<-as.data.frame(closeness(x)) %>% rownames_to_column("Auditor_name") %>% 
    rename(closeness=`closeness(x)`) %>% arrange(-closeness) %>% slice(1:5) %>% 
    rowid_to_column("group")
}


joining<-function(x,y){
  nodes<-left_join(x,y, by=c("label"="Auditor_name")) %>% select(-closeness) %>%
    mutate(group=as.character(group))
}


boom<-fluk %>% mutate(boom=map(data, flah),
                      nodes=map(data, klah),
                      edges=map2(boom,nodes, blah),
                      closeness=map(boom, close),
                      nodes=map2(nodes, closeness, joining)) 




# Making the Network Graph for each BG ------------------------------------


lapply(1:11, function(x)
  visNetwork(boom$nodes[[x]], boom$edges[[x]], height = "1500px", width = "100%", 
                                         main = boom$Ownership_group[[x]]) %>% 
                visOptions(highlightNearest = list(enabled=TRUE, degree=1, hover=TRUE),
                           selectedBy = "group",
                           nodesIdSelection = TRUE) %>% 
                visGroups(groupname = "1", color="red") %>%
                visGroups(groupname = "2", color="violet") %>%
                visGroups(groupname = "3", color="yellow") %>% 
                visGroups(groupname = "4", color="green") %>% 
                visGroups(groupname = "5", color="maroon")%>% 
                visLayout(randomSeed = 123) %>%
                visSave(file = paste0(boom$Ownership_group[[x]], ".html")))






