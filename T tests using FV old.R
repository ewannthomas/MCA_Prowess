

library(tidyverse)
library(modelsummary)
library(broom)
library(gt)


load("./Prowess/R Data Prowess/FV Old.Rdata")

# T test of Centrality measure and audit count ----------



vlah<-c("Audit_count", "AC_big4", "Closeness","Degree", "Eigen_centrality", 
        "Closeness_per_year","Degree_per_year","Eigen_centrality_per_year", 
        "Closeness_Mkt","Degree_Mkt","Eigen_centrality_Mkt")


T_test<-lapply(vlah, function(y)
  
  t.test(FV[[y]] ~ FV[["Aud_rotation"]], var.equal=FALSE, alternative= "two.sided"))

T_test<-set_names(T_test, vlah)


T_test<-map_dfr(vlah, ~broom::tidy(T_test[[.x]]), .id="Variable") %>% select(-7:-11) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  rename(Mean_Difference = estimate,
         Post_aud_rotation_mean = estimate2,
         Pre_aud_rotation_mean = estimate1) %>% 
  mutate(Variable = vlah)


gt::gt(T_test) %>% 
  tab_header(title = "Two Sample T-Test of Centrality Measures and Number of Audits",
             subtitle = "Pre vs Post Audit Rotation") %>%
  tab_source_note(source_note = c("All values are rounded off to three decimal places",
                                  "On the unfiltered sample"))


rm(T_test, vlah)


