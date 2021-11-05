
library(tidyverse)


# Audit Quality Measure Creation -------------------------------------------------
 


FV<-FV %>% mutate(NIC_2 = case_when(
  is.na(Nic_code) ~ NA_real_,
  Nic_code >10 ~ as.numeric(str_sub(as.character(Nic_code), end = 2L)),
  Nic_code <10 ~ Nic_code)) %>% 
  group_by(Co_name) %>% 
  mutate(across(c(Sales, Receivables, Inventories), ~ . - lag(., n=1L), 
                .names = "Chg_{.col}")) %>% ungroup() %>% 
  rowwise() %>%
  mutate(Non_Audit_Fee =  sum(c(Auditor_fees_taxation, Auditor_fees_companylaw, Cnsltncy_fees_auditors), na.rm = T),
         Accruals = diff(c(PBDITA, Cfo), na.rm=T),
         Production = sum(c(Cost_goods_sold, Chg_Inventories), na.rm = T),
         Disc_expenses = sum(c(Sellndistr_expenses, Trvl_expenses, Rnd_expense), na.rm = T)) %>% 
  ungroup()

  
FV<-FV %>% mutate(across(c(Production, Disc_expenses, Cfo, Accruals, Gppe, Sales, Curr_assets, 
                  curr_liab, PBDITA, Chg_sales, Chg_Receivables, Chg_Inventories ), ~ ./Lag_Tot_assets, .names = "{.col}_at")) %>%
  group_by(NIC_2, year) %>%
  mutate(across(c(Curr_assets, curr_liab, Size, PBDITA, PB, ends_with("at")), ~ . - mean(., na.rm=T), 
                .names = "{.col}_ind_mean")) %>% ungroup() %>% 
  group_by(Co_name) %>% 
  mutate(across(c(Cfo_at, Chg_sales_at, Production_at, Disc_expenses_at), ~ lag(., n=1L), .names = "Lag_{.col}")) %>% ungroup()

FV<-FV %>% mutate(Sales_at_minus_rec_at = Chg_sales_at - Chg_Receivables_at)

FV<-FV %>% mutate(Chg_sales_at_t1 = lead(Chg_sales_at, n=1L))


# FV filters ----------------------------------------

#From now onwards, we will call FV with 451 vars & 40,656 obs as FV_old and newly filtered data as FV
FV_old<-FV




#FV filter by incorp year and Industry type
FV<-FV %>% filter(Incorp_year<2000,  Indust_type==1) %>% 
  filter(!Ownership_group %in% c("Central Govt. - Commercial Enterprises", "State Govt. - Commercial Enterprises", 
                                 "State Bank of India Group", "State and Private sector")) %>%
  mutate(Log_Age=log(year-Incorp_year))






#Filtering out companies without sales value for atleast two years
Sales_NA_filter<-FV %>% group_by(Prowess_code) %>% 
  count(is.na(Sales)) %>% 
  filter(`is.na(Sales)`==TRUE) %>% filter(n>2) %>% 
  select(-`is.na(Sales)`) %>% 
  rename(Sales_NA_filter = n)

FV<-FV %>% left_join(., Sales_NA_filter) %>% mutate(Sales_NA_filter = case_when(
  is.na(Sales_NA_filter) ~ 1,
  Sales_NA_filter>2 ~ 0,
  TRUE ~ 99))

FV<-FV %>% filter(Sales_NA_filter==1)
remove(Sales_NA_filter)



#Creating NIC_Count by year to filter out indus groups with 10 or less firms each year
FV<-FV %>% group_by(year, NIC_2) %>% count(NIC_2) %>% ungroup() %>% 
  rename(NIC_count =n) %>% 
  left_join(FV, .)




# FV Descriptive ----------------------------------------------------------



FV_desc<-FV %>% filter(Indust_type==1) %>% group_by(Groupstand) %>% 
  summarise(across(c("Sales", "Tot_assets", "Mkt_capital", "Total_liab", "Curr_assets", "curr_liab", "Profit_after_tax",
                     "currentratio", "Cfo", "Roa", "Roce", "Ronw", "Prom_percent_shares_held", "debtequity", "total_capital",
                     "Gppe", "Tot_expenses", "Production", "Disc_expenses", "PB", "Audit_fees", "Non_Audit_Fee", "Accruals"), list(Mean= ~ mean(., na.rm=T), Median= ~ median(., na.rm = T), sd= ~ sd(.,na.rm = T), Min= ~ min(., na.rm=T), Max= ~ max(., na.rm=T)), .names = "{.col}.{.fn}")) %>% 
  pivot_longer(2:116,names_to = "var", values_to = "vals") %>% 
  mutate(Stat = str_extract(var, pattern = c("Mean", "Median", "sd", "Min", "Max")),
         var = str_extract(var, "([:alpha:]*\\_[:alpha:]*|[:alpha:]*)"),
         vals = round(vals,2)) %>% 
  pivot_wider(names_from = "Stat", values_from = "vals") %>% 
  pivot_wider(names_from="Groupstand", values_from= c("Mean", "Median", "sd", "Min", "Max"))


FV_desc_gt<-FV_desc %>% gt::gt() %>% tab_spanner(label= "Mean", columns= matches("Mean")) %>% 
  tab_spanner(label= "Median", columns= matches("Median")) %>% 
  tab_spanner(label= "Std. Deviation", columns= matches("sd")) %>% 
  tab_spanner(label= "Minimum", columns= matches("Min")) %>% 
  tab_spanner(label= "Maximum", columns= matches("Max")) %>% 
  tab_header(title = "Descriptive Statistics of Financial Variables",
             subtitle = "Statistics Grouped by Group Stand Values") %>% 
  cols_label(Mean_0 = "0",
             Mean_1 = "1",
             Median_0 = "0",
             Median_1 = "1",
             sd_0 = "0",
             sd_1 = "1",
             Min_0 = "0",
             Min_1 = "1",
             Max_0 = "0",
             Max_1 = "1") %>% 
  tab_source_note(md("All values are rounded off to two decimal places"))





# REM Regressions ---------------------------------------------------------

#Creating NIC_count.10 vector
NIC<-FV %>% filter(NIC_count>10) %>% distinct(NIC_2) %>% pull()

#creating Year 2013 + vector
Year<-FV %>% filter(year>=2013) %>% distinct(year) %>% pull()

reg_fn<-lapply(NIC, function(i)
  lapply(Year, function(j)  FV %>% filter(NIC_2==i & year==j) %>% 
           lm(Production_at ~ One_at + Sales_at + Chg_sales_at + Lag_Chg_sales_at, data=., na.action = na.exclude)))

# Accrual M Jones Model ---------------------------------------------------
boom=FV # assigning FV to a temporary data object 

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

MJ_model<-function(x){
  lm(Accruals_at ~ One_at + Sales_at_minus_rec_at + Gppe_at , data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, MJ_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, resid) %>% left_join(FV,.) %>% 
  rename(MJ_resid=resid)
remove(boom, MJ_model) 


# MJ Model ROA ------------------------------------------------------------
boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

MJ_model_roa<-function(x){
  lm(Accruals_at ~ One_at + Sales_at_minus_rec_at + Gppe_at + Lag_Roa , data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, MJ_model_roa),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, resid) %>% left_join(FV,.) %>% 
  rename(MJ_roa_resid=resid)
remove(boom, MJ_model_roa) 

MJ_model<-list(
  "MJ_Model" = FV %>% lm(MJ_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Log_Age+as.factor(NIC_2)+as.factor(year), data = .),
  "MJ_Model_ROA" = FV %>% lm(MJ_roa_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Log_Age+as.factor(NIC_2)+as.factor(year), data = .))
library(modelsummary)
msummary(c(MJ_model), stars = T)



# Roy Prod Model ----------------------------------------------------------


boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

Roy_Prod_model<-function(x){
  lm(Production_at ~ One_at + Sales_at + Chg_sales_at + Lag_Chg_sales_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Roy_Prod_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, resid) %>% left_join(FV,.) %>% 
  rename(Roy_prod_resid=resid)
remove(boom, Roy_Prod_model) 

# Roy CFO Model -----------------------------------------------------------

boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

Roy_CFO_model<-function(x){
  lm(Cfo_at ~ One_at + Sales_at + Chg_sales_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Roy_CFO_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, resid) %>% left_join(FV,.) %>% 
  rename(Roy_cfo_resid=resid)
remove(boom, Roy_CFO_model) 


# Roy Disc Model ----------------------------------------------------------
boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

Roy_Disc_model<-function(x){
  lm(Disc_expenses_at ~ One_at + Sales_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Roy_Disc_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, resid) %>% left_join(FV,.) %>%
  rename(Roy_disc_resid=resid)
remove(boom, Roy_Disc_model) 




Roy_Models<-list(
  "Roy_Prod_Model" = FV %>% lm(Roy_prod_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Log_Age+MJ_roa_resid+as.factor(NIC_2)+as.factor(year), data = .),
  "Roy_CFO_Model" = FV %>% lm(Roy_cfo_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Log_Age+MJ_roa_resid+as.factor(NIC_2)+as.factor(year), data = .),
  "Roy_Disc_Model" = FV %>% lm(Roy_disc_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Log_Age+MJ_roa_resid+as.factor(NIC_2)+as.factor(year), data = .))

library(modelsummary)
msummary(c(MJ_model,Roy_Models), stars = T)



# Vastav Prod Model ----------------------------------------------------------


boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004 & year<=2019) %>% group_by(NIC_2, year) %>% nest()

Vastav_Prod_model<-function(x){
  lm(Production_at ~ One_at + Sales_at + Chg_sales_at + Lag_Chg_sales_at + Size + PB + Lag_Roa + Chg_sales_at_t1 + Lag_Production_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Vastav_Prod_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))



FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, resid) %>% left_join(FV,.) %>% 
  rename(Vastav_prod_resid=resid)
remove(boom, Vastav_Prod_model) 

# Vastav CFO Model -----------------------------------------------------------

boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004 & year<=2019) %>% group_by(NIC_2, year) %>% nest()

Vastav_CFO_model<-function(x){
  lm(Cfo_at ~ One_at + Sales_at + Chg_sales_at + Lag_Chg_sales_at + Size + PB + Lag_Roa + Chg_sales_at_t1 + Lag_Cfo_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Vastav_CFO_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, resid) %>% left_join(FV,.) %>% 
  rename(Vastav_cfo_resid=resid)
remove(boom, Vastav_CFO_model) 


# Vastav Disc Model ----------------------------------------------------------
boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004 & year<=2019) %>% group_by(NIC_2, year) %>% nest()

Vastav_Disc_model<-function(x){
  lm(Disc_expenses_at ~ One_at + Sales_at + Chg_sales_at + Lag_Chg_sales_at + Size + PB + Lag_Roa + Chg_sales_at_t1 + Lag_Disc_expenses_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Vastav_Disc_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, resid) %>% left_join(FV,.) %>%
  rename(Vastav_disc_resid=resid)
remove(boom, Vastav_Disc_model) 




Vastav_Models<-list(
  "Vastav_Prod_Model" = FV %>% lm(Vastav_prod_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Log_Age+MJ_roa_resid+as.factor(NIC_2)+as.factor(year), data = .),
  "Vastav_CFO_Model" = FV %>% lm(Vastav_cfo_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Log_Age+MJ_roa_resid+as.factor(NIC_2)+as.factor(year), data = .),
  "Vastav_Disc_Model" = FV %>% lm(Vastav_disc_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Log_Age+MJ_roa_resid+as.factor(NIC_2)+as.factor(year), data = .))

library(modelsummary)
msummary(c(Roy_Models, MJ_model, Vastav_Models), stars = T)




FV<-FV %>% select(-starts_with("Roy_"), -starts_with("Vastav"), -starts_with("MJ_"))
# Saving Image ------------------------------------------------------------
save.image("./Prowess/Prowess Data.Rdata")

