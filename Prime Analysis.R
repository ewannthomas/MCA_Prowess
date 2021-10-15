

library(tidyverse)


# Reading in Prime Data ---------------------------------------------------


load("./Prime/Prime Data.Rdata")

# AGM Resolutions Counting ------------------------------------------------


Resolution_Purposes<-AGM %>% distinct(Resolution_purpose)


Resolution_purpose_count<-AGM %>% group_by(Co_name, year, Type_of_meeting)%>% count(Resolution_purpose) %>% 
  mutate(Resolution_Percentage = (n/sum(n))*100) %>% rename(Resolution_Frequency = n)


Meeting_type_count<-AGM %>% group_by(Co_name, year) %>% count(Type_of_meeting) %>% 
  mutate(Meeting_Percentage = (n/sum(n))*100) %>% rename(Meeting_Frequency = n)

Resolution_purpose_count<-AGM %>% group_by(Co_name, year, Type_of_meeting)%>% count(Resolution_purpose) %>% 
  group_by(Co_name, year) %>% 
  mutate(Percentage = (n/sum(n))*100) %>% rename(Frequency = n)


# Board Dependence Percentage ---------------------------------------------

Board_Indep<-Prime_Director %>% group_by(Nse_symbol, year) %>%  
  count(Independent) %>% 
  mutate(indep_percent = round(n/sum(n)*100,2)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = "Independent", values_from = "indep_percent") %>% 
  rename(Indep_perc = `1`,
         Non_Indep_perc = `0`,
         Indep_unknown_perc = `NA`)


Female_dir_count<-Prime_Director %>% group_by(Nse_symbol, year) %>% count(Female) %>% 
  mutate(female_percent =  round(n/sum(n)*100,2)) %>% select(-n) %>% 
  pivot_wider(names_from = "Female", values_from = "female_percent") %>% 
  rename(Male_dir_perc = `0`,
         Female_dir_perc = `1`)

Indep_fem_count<-full_join(Board_Indep, Female_dir_count)

remove(Board_Indep, Female_dir_count)

# Board Dependence Percentage using prowess board listed and unlisted data---------------------------------------------

Prowess_Board_List_Unlist<-Prowess_Board_List_Unlist %>% mutate(Independent=case_when(
  Independent_Non_independent_classification=="Independent" ~ 1,
  Independent_Non_independent_classification=="Non-independent" ~ 0))

Board_Indep_prow<-Prowess_Board_List_Unlist %>% group_by(Co_name, year) %>%  
  count(Independent) %>% 
  mutate(indep_percent = round(n/sum(n)*100,2)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = "Independent", values_from = "indep_percent") %>% 
  rename(Indep_perc = `1`,
         Non_Indep_perc = `0`,
         Indep_unknown_perc = `NA`)








# CEO Duality -------------------------------------------------------------
Prowess_Board_List_Unlist<-Prowess_Board_List_Unlist %>% mutate(CEO_dual = case_when(
  Designation_category=="CMD - Chairperson & Managing Director" ~ 1,
  TRUE ~ NA_real_
))

CEO_Dual<-Prowess_Board_List_Unlist %>% group_by(Co_name, year) %>% 
  distinct(CEO_dual) %>% filter(CEO_dual==1) %>% ungroup()





# Remuneration Co_year clubbing -------------------------------------------
Dir_Remuneration_Sum<-Prowess_Board_List_Unlist %>% group_by(Co_name, year) %>% 
  summarise(across(c("Salary", "Directors_sitting_fees", "Contribution_to_provident_fund",
                   "Bonus_Commission", "Perquisites", "Retirement_benefits", "Stock_Options_Amt",
                   "Other_Remuneration", "Total_remuneration"), list(Sum= ~ sum(., na.rm = T)),.names = "{.col}_{.fn}")) %>% 
  ungroup()

# Director Count & Merge: CEO_Dual, Dir count, Dir_remun----------------------------------------------------------

Dir_count<-Prowess_Board_List_Unlist %>% group_by(Co_name, year) %>% 
  count(year) %>% rename(Director_count = n)


Dir_Remuneration_Sum<-Dir_Remuneration_Sum %>% 
  left_join(., Dir_count) %>% 
  left_join(., CEO_Dual)

remove(Dir_count, CEO_Dual)



# Saving object for merging with FV ---------------------------------------


save(Board_Indep_prow,Indep_fem_count, Dir_Remuneration_Sum, file = "./Prime/Object for merge with FV.Rdata")

# Saving Image ------------------------------------------------------------
save.image("./Prime/Prime Data.Rdata")

