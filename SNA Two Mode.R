
library(tidyverse)
library(igraph)
library(broom)
library(gt)


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





fluk<-FV %>% group_by(Ownership_group) %>% nest() %>% na.exclude()




flah<-function(x){
  
  Edge<-x %>%  
    group_by(Co_name) %>% 
    distinct(Auditor_name) %>% 
    na.exclude()
  
  edge<-graph_from_data_frame(Edge)
  
  V(edge)$type<-bipartite.mapping(edge)$type
  
  edge<-bipartite.projection(edge, multiplicity = TRUE, which = "true")
  
  closeness<-as.data.frame(closeness(edge)) %>% rownames_to_column("Auditor_name") %>% 
    rename(Closeness = `closeness(edge)`)
  
  eigen<-as.data.frame(eigen_centrality(edge)) %>% 
    rownames_to_column("Auditor_name") %>% select(1,2) %>% 
    rename(Eigen_centrality = vector)
  
  Degree<-as.data.frame(degree(edge)) %>% rownames_to_column("Auditor_name") %>% 
    rename(Degree = `degree(edge)`)
  
  bluh<-left_join(Edge,closeness) %>%
    left_join(., eigen) %>% 
    left_join(.,Degree)
  
  
}


boom<-fluk %>% mutate(boom=map(data, flah)) %>% select(-data) %>% unnest(boom)

SNA<-FV %>% left_join(.,boom)



# Centrality by year and ownership group --------------------------------

fluk<-FV %>% group_by(Ownership_group, year) %>% nest() %>% na.exclude()

flah<-function(x){
  
  Edge<-x %>%  
    group_by(Co_name) %>% 
    distinct(Auditor_name) %>% 
    na.exclude()
  
  edge<-graph_from_data_frame(Edge)
  
  V(edge)$type<-bipartite.mapping(edge)$type
  
  return(edge)
}


blah<-function(x){
  
  x<-bipartite.projection(x, multiplicity = TRUE, which = "true")
  
  closeness<-as.data.frame(closeness(x)) %>% rownames_to_column("Auditor_name") %>% 
    rename(Closeness_per_year = `closeness(x)`)
  
  eigen<-as.data.frame(eigen_centrality(x)) %>% 
    rownames_to_column("Auditor_name") %>% select(1,2) %>% 
    rename(Eigen_centrality_per_year = vector)
  
  Degree<-as.data.frame(degree(x)) %>% rownames_to_column("Auditor_name") %>% 
    rename(Degree_per_year = `degree(x)`)
  
  
  bluh<-left_join(closeness, eigen) %>% 
    left_join(., Degree)
}


vlah<-function(x,y){
  glah<-left_join(x,y)
}


boom<-fluk %>% mutate(boom=map(data, flah),
                      boom=map_if(boom, is.bipartite, blah, .else = NULL)) %>% 
  filter(sapply(boom, is.data.frame)==TRUE)


boom<-boom %>% mutate(def = map2(data, boom, vlah)) %>% 
  select(Ownership_group, year, def) %>% unnest(def) 


SNA<-SNA %>% left_join(.,boom)


rm(fluk, boom, flah, blah, vlah)




# Centrality at Mkt Level -------------------------------------------------


Edge<-FV %>%  
  group_by(Co_name) %>% 
  distinct(Auditor_name) %>% 
  na.exclude()

edge<-graph_from_data_frame(Edge)

V(edge)$type<-bipartite.mapping(edge)$type

edge<-bipartite.projection(edge, multiplicity = TRUE, which = "true")

closeness<-as.data.frame(closeness(edge)) %>% rownames_to_column("Auditor_name") %>% 
  rename(Closeness_Mkt = `closeness(edge)`)

eigen<-as.data.frame(eigen_centrality(edge)) %>% 
  rownames_to_column("Auditor_name") %>% select(1,2) %>% 
  rename(Eigen_centrality_Mkt = vector)

Degree<-as.data.frame(degree(edge)) %>% rownames_to_column("Auditor_name") %>% 
  rename(Degree_Mkt = `degree(edge)`)

SNA<-left_join(closeness, eigen) %>% 
  left_join(.,Degree) %>% 
  left_join(SNA, .)

rm(Edge, edge, closeness, eigen, Degree)

# Centrality T-test -------------------------------------------------

vlah<-c("Closeness","Degree", "Eigen_centrality", "Closeness_per_year","Degree_per_year","Eigen_centrality_per_year", "Closeness_Mkt","Degree_Mkt","Eigen_centrality_Mkt")

klah<-c("Groupstand", "Top_10", "Top_50", "Top_10_standalone")

T_test<-lapply(klah, function(y)
  lapply(vlah, function(x)
    
    t.test(SNA[[x]] ~ SNA[[y]], var.equal=FALSE, alternative= "two.sided")))


T_test<-lapply(1:4, function(x)
  set_names(T_test[[x]], nm=vlah))

T_test<-set_names(T_test, klah)


T_test[[1]]<-map_dfr(T_test[[1]], ~broom::tidy(.), .id="Variable") %>% select(-7:-11) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  rename(Mean_Difference = estimate,
         BG_Mean = estimate2,
         Stand_Alone_Mean = estimate1)

T_test[[2]]<-map_dfr(T_test[[2]], ~broom::tidy(.), .id="Variable") %>% select(-7:-11) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  rename(Mean_Difference = estimate,
         Top_10_BG_Mean = estimate2,
         Other_BG_Mean = estimate1)


T_test[[3]]<-map_dfr(T_test[[3]], ~broom::tidy(.), .id="Variable") %>% select(-7:-11) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  rename(Mean_Difference = estimate,
         Top_50_BG_Mean = estimate2,
         Other_BG_Mean = estimate1)


T_test[[4]]<-map_dfr(T_test[[4]], ~broom::tidy(.), .id="Variable") %>% select(-7:-11) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  rename(Mean_Difference = estimate,
         Top_10_BG_Mean = estimate2,
         Stand_Alone_Mean = estimate1)

rm(vlah, klah)

gt::gt(T_test[[1]]) %>% 
  tab_header(title = "Two Sample T-Test of Centrality Measures",
             subtitle = "Business Groups vs Standalones") %>%
  tab_source_note(md("All values are rounded off to three decimal places"))

gt::gt(T_test[[2]]) %>% 
  tab_header(title = "Two Sample T-Test of Centrality Measures",
             subtitle = "Top 10 Business Groups vs Other Business Groups") %>%
  tab_source_note(md("All values are rounded off to three decimal places"))

gt::gt(T_test[[3]]) %>% 
  tab_header(title = "Two Sample T-Test of Centrality Measures",
             subtitle = "Top 50 Business Groups vs Other Business Groups") %>%
  tab_source_note(md("All values are rounded off to three decimal places"))

gt::gt(T_test[[4]]) %>% 
  tab_header(title = "Two Sample T-Test of Centrality Measures",
             subtitle = "Top 10 Business Groups vs Standalones") %>%
  tab_source_note(md("All values are rounded off to three decimal places"))

rm(FV)


# Top 50 Auditors ---------------------------------------------------------


FV<-FV_old %>%select(Ownership_group, Co_name,year,Auditor_name, Auditor_name_big4, Aud_count, Aud_count_big4, 
                     Indust_type,Auditor_fees,Audit_fees,Auditor_fees_taxation,
                     Auditor_fees_companylaw,Cnsltncy_fees_auditors, Aud_tenure, Aud_rotation, 
                     starts_with(c("Closeness", "Eigen", "Degree"))) 



vlah<-c("Closeness", "Eigen_centrality", "Degree")

blah<-function(x){
}

Aud_50_centrality<-map(vlah, ~ FV %>% arrange(desc(.x)) %>% 
                         distinct(across(c("Auditor_name", .x))))


slice(1:100)
group_by(Auditor_name) %>% 
  summarise(.x=max(.x)) %>% 
  arrange(desc(.x)) %>% 
  slice(1:50)) 


FV %>% arrange(across(ends_with("Closeness") , desc))
distinct(across(.cols = c("Auditor_name", "Closeness"))) %>% 
  slice(1:100) %>% 
  group_by(Auditor_name) %>% 
  summarise(across(c("Closeness"), list("Closeness"= ~max(Closeness)), .names = "Closeness")) %>% 
  arrange(across(.cols = "Closeness", desc)) %>% 
  slice(1:50)








# Saving Image ------------------------------------------------------------
save.image("./Prowess/R Data Prowess/SNA.Rdata")

