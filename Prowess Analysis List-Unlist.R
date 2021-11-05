
library(tidyverse)
library(modelsummary)
library(broom)
library(gt)


# Reading in FV List-Unlist -----------------------------------------------
load("./Prowess/R Data Prowess/FV List-Unlist.Rdata")

rm(Aud_NSE, Aud_NSE_un)

# Creating Lags in FV -----------------------------------------------------
FV<-FV %>% group_by(Co_name) %>% 
  mutate(across(c("Roa", "Tot_assets"), ~ lag(.,n=1L), .names = "Lag_{.col}")) %>% 
  ungroup()


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
         Accruals = diff(c(PAT, Cfo), na.rm=T),
         Production = sum(c(Cost_goods_sold, Chg_Inventories), na.rm = T),
         Disc_expenses = sum(c(Sellndistr_expenses, Trvl_expenses, Rnd_expense), na.rm = T)) %>% 
  ungroup() %>% mutate(
    Mkt_capital = case_when(
      Mkt_capital==0 ~ NA_real_,
      TRUE ~ Mkt_capital),
         Size=log(Mkt_capital),
         One_at=1/Lag_Tot_assets) 

FV<-FV %>% group_by(Ownership_group,year) %>% mutate(Tot_assets_sum=sum(Tot_assets,na.rm = T),
                                               group_quantile=ntile(Tot_assets_sum,10)) %>% ungroup()

FV<-FV %>% mutate(Log_total_assets=log(Tot_assets))
  

FV<-FV %>% mutate(across(c(Production, Disc_expenses, Cfo, Accruals, Gppe, Sales, Curr_assets, 
                  curr_liab, PBDITA, Chg_Sales, Chg_Receivables, Chg_Inventories ), ~ ./Lag_Tot_assets, .names = "{.col}_at")) %>%
  group_by(NIC_2, year) %>%
  mutate(across(c(Curr_assets, curr_liab, Size, PBDITA, PB, Promoters_percent_shares_held, ends_with("at")), ~ . - mean(., na.rm=T), 
                .names = "{.col}_ind_mean")) %>% ungroup() %>% 
  group_by(Co_name) %>% 
  mutate(across(c(Cfo_at, Chg_Sales_at, Production_at, Disc_expenses_at), ~ lag(., n=1L), .names = "Lag_{.col}")) %>% ungroup()

FV<-FV %>% mutate(Sales_at_minus_rec_at = Chg_Sales_at - Chg_Receivables_at)

FV<-FV %>% mutate(Chg_Sales_at_t1 = lead(Chg_Sales_at, n=1L))

FV <- FV %>% mutate(Age=abs(Incorp_year-year)) #creating age of firms

FV <- FV %>% mutate(Invrec_at=(Inventories/Lag_Tot_assets)+(Receivables/Lag_Tot_assets))

FV<-FV %>% mutate(Audit_fees_at = (Audit_fees+0.01)/Lag_Tot_assets)

FV<- FV %>% mutate(Non_Audit_Fee_at= (Non_Audit_Fee+0.01)/Lag_Tot_assets)

FV<- FV %>% mutate(Time_dummy = case_when(  #Creating pre and post law dummy for auditor rotation
  year>=2011 & year<=2014 ~ 0,
  year>=2015 & year<=2018 ~ 1,
  TRUE ~ NA_real_))

##Audit Concentration measures creation


FV<-FV %>% group_by(Co_name, year) %>% #Creating Joint Auditor Dummy 
  count(year) %>% left_join(FV,.) %>% 
  mutate(Joint_Auditor = case_when(
    n==2 ~ 2,
    n==3 ~ 3,
    n==1 ~ 0,
    is.na(n) ~ 0)) %>% select(-n)

FV<-FV %>% group_by(Auditor_name, year) %>% 
  mutate(Aud_sales=sum(Sales, na.rm = T)) %>% ungroup() %>% 
  group_by(NIC_2, year) %>% 
  mutate(Ind_sales = sum(Sales, na.rm = T),
         Audspec = Aud_sales/Ind_sales) %>% 
  group_by(Ownership_group, year, Auditor_name) %>%  #creating auditor concentration
  mutate(Aud_count = n()) %>% 
  group_by(Ownership_group, year) %>% 
  mutate(Ownership_group_count = n(),
         AC = Aud_count/Ownership_group_count) %>% 
  mutate(Auditor_name_big4 = case_when(  #creating big4 auditor concentration
    big4type=="Others" ~ Auditor_name,
    TRUE ~ big4type)) %>% 
  group_by(Ownership_group, year, Auditor_name_big4) %>% 
  mutate(Aud_count_big4 = n(),
         AC_big4 = Aud_count_big4/Ownership_group_count) %>% 
  group_by(Co_name, Auditor_name) %>%  #creating Audit tenure and GTENURE
  mutate(Aud_tenure= case_when(
    !is.na(Auditor_name) ~ row_number(), 
    TRUE ~ NA_integer_)) %>% 
  group_by(Ownership_group, Auditor_name) %>% mutate(GTENURE = mean(Aud_tenure, na.rm=T))


FV<-FV %>% group_by(Ownership_group, Co_name) %>% #Creating auditor rotation
  mutate(Aud_rotation=case_when(
    Joint_Auditor==0 & lag(Auditor_name, n=1L) != Auditor_name ~ 1,
    Joint_Auditor==0 & lag(Auditor_name, n=1L) == Auditor_name ~ 0,
    Joint_Auditor==2 & lag(Auditor_name, n=2L) != Auditor_name ~ 1,
    Joint_Auditor==3 & lag(Auditor_name, n=3L) != Auditor_name ~ 1,
    Joint_Auditor==2 & lag(Auditor_name, n=2L) == Auditor_name ~ 0,
    Joint_Auditor==3 & lag(Auditor_name, n=3L) == Auditor_name ~ 0
  ))


FV<-FV %>% group_by(Co_name) %>% 
  mutate(Aud_rotation = case_when(
    is.na(Aud_rotation) ~ 0, 
    TRUE ~ Aud_rotation),
    Aud_rotation=case_when(
      is.na(Auditor_name) ~ NA_real_,
      TRUE ~ Aud_rotation
    ))


FV<-FV %>% group_by(Co_name) %>% ##creating share OS increment percent and incre > 10%
  mutate(Share_OS_increment_perc=round((Shares_OS-lag(Shares_OS, n=1L))/lag(Shares_OS, n=1L)*100, 2),
         EQFIN = case_when(
           is.na(Share_OS_increment_perc) ~ NA_real_,
           Share_OS_increment_perc >=10 ~ 1,
           TRUE ~ 0))


FV<-FV %>% group_by(Ownership_group, year) %>% #Creating Group level variables
  mutate(GNLAF = log(sum(Audit_fees)),
         GNLNAF = log(sum(Non_Audit_Fee)),
         GNAFR = sum(Non_Audit_Fee)/sum(Auditor_fees),
         GSIZE = sum(Mkt_capital),
         GAUDSPEC = case_when(
          Audspec > median(Audspec, na.rm = T) ~ 1,
          TRUE ~ 0),
         GPROMHOL = mean(Promoters_percent_shares_held, na.rm=T),
         GLOSS = case_when(
           any(PBDITA < 0) ~ 1,
           TRUE ~ 0),
         GEQFIN = case_when(
           sum(EQFIN) >= 1 ~ 1,
           TRUE ~ 0)) %>% 
  group_by(Co_name, year) %>% # creating LOSS at co and year level
  mutate(LOSS= case_when(
    PBDITA < 0 ~ 1,
    TRUE ~ 0)) 

FV<-FV %>% mutate(Aud_elig_dummy = case_when( # Creating eligibility dummy
  year>=2014 & Aud_tenure >= 10 ~ 1,
  year<2014 & Aud_tenure >= 10 ~ 0,
  Aud_tenure < 10 ~ 0,
  is.na(Aud_tenure) ~ NA_real_),
  Aud_elig_dummy = case_when(
    year>=2014 & Aud_tenure >= 10 ~ 1,
    Aud_tenure < 10 ~ 0,
    is.na(Aud_tenure) ~ NA_real_)) %>% 
  group_by(Co_name) %>% mutate(Aud_Treatment = case_when(
    Aud_elig_dummy==1 & lead(Aud_rotation)==1 & year>2014 & year<2019 ~ 1,
    Aud_elig_dummy==0 & lead(Aud_rotation)==1 & year>2014 & year<2019 ~ 0)) %>%  fill(Aud_Treatment, .direction = c("updown")) %>%
  mutate(Aud_Treatment = case_when(
    year>=2019 | year<=2010 ~ NA_real_,
    TRUE ~ Aud_Treatment)) %>% 
  ungroup()

FV<-FV %>% group_by(Auditor_name) %>% mutate(Audit_count=n()) %>% 
  ungroup() # Creating number of audits done per auditor

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))



#Creating Auditor Reappointment
temp_data<-FV %>% group_by(Ownership_group, Co_name) %>% 
  mutate(Aud_reappoint = case_when(
    Joint_Auditor==0 & lead(Aud_rotation)==1 ~ Auditor_name,
    Joint_Auditor==0 & Aud_rotation==1 ~ Auditor_name,
    Joint_Auditor==2 & lead(Aud_rotation, n=2L)==1 ~ Auditor_name,
    Joint_Auditor==2 & Aud_rotation==1 ~ Auditor_name,
    Joint_Auditor==3 & lead(Aud_rotation, n=3L)==1 ~ Auditor_name,
    Joint_Auditor==3 & Aud_rotation==1 ~ Auditor_name)) %>% 
  select(Ownership_group, Co_name, year, Auditor_name, Aud_rotation, Aud_reappoint) %>%
  group_by(Ownership_group) %>% 
  nest()




#function to pull a set of rotated and non rotated audiotrs
temp_fn<-function(x){
  
  y<-intersect(x %>% filter(Aud_rotation==0) %>% pull(Aud_reappoint) %>% unique() %>% na.exclude(), 
               x %>% filter(Aud_rotation==1) %>% pull(Aud_reappoint) %>% unique() %>% na.exclude()) 
}





temp_data<-temp_data %>% mutate(vectors= map(data,temp_fn)) %>% unnest(data) 




temp_data$Aud_reappoint<-sapply(1:214230, function(x)
  temp_data$Auditor_name[[x]] %in% temp_data$vectors[[x]] 
)


temp_data<-temp_data %>% select(-vectors) %>%
  mutate(Aud_reappoint=case_when(
    Aud_reappoint==TRUE ~ 1,
    is.na(Auditor_name) ~ NA_real_,
    TRUE ~ 0
  ))


FV<-temp_data %>% select(-Aud_rotation) %>% left_join(FV,. )

rm(temp_data, temp_fn)

#Creating specialized and non specialized reappointment
FV<-FV %>% mutate(Spec_Aud_reappoint=case_when(
  Aud_reappoint==1 & GAUDSPEC==1 ~ 1,
  Aud_reappoint==0 & GAUDSPEC==1 ~ 0,
  Aud_reappoint==1 & GAUDSPEC==0 ~ 1,
  Aud_reappoint==0 & GAUDSPEC==0 ~ 0,
  TRUE ~ NA_real_))


eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))





#Creating Auditor Reappointment spec & nonspec classification
temp_data<-FV %>% group_by(Ownership_group, Co_name) %>% 
  mutate(Aud_reappoint = case_when(
    Joint_Auditor==0 & lead(Aud_rotation)==1 ~ Auditor_name,
    Joint_Auditor==0 & Aud_rotation==1 ~ Auditor_name,
    Joint_Auditor==2 & lead(Aud_rotation, n=2L)==1 ~ Auditor_name,
    Joint_Auditor==2 & Aud_rotation==1 ~ Auditor_name,
    Joint_Auditor==3 & lead(Aud_rotation, n=3L)==1 ~ Auditor_name,
    Joint_Auditor==3 & Aud_rotation==1 ~ Auditor_name)) %>% 
  select(Ownership_group, Co_name, year, Auditor_name, Aud_rotation, Aud_reappoint, GAUDSPEC) %>%
  group_by(Ownership_group) %>% 
  nest() # running modified reapp codes for creating spec to spec and other reapps


temp_fn<-function(x){
  
  y<-intersect(x %>% filter(Aud_rotation==0) %>% pull(Aud_reappoint) %>% unique() %>% na.exclude(), 
               x %>% filter(Aud_rotation==1) %>% pull(Aud_reappoint) %>% unique() %>% na.exclude()) 
}




temp_data<-temp_data %>% mutate(vectors= map(data,temp_fn)) %>% unnest(data) %>% 
  mutate(Aud_reappoint=case_when(
    Aud_rotation==1 ~ Aud_reappoint,
    Aud_rotation==0 ~ NA_character_ # modofied here
  ))





temp_data$Aud_reappoint<-sapply(1:214230, function(x)
  temp_data$Aud_reappoint[[x]] %in% temp_data$vectors[[x]] #modified here
)


temp_data<-temp_data %>% select(-vectors)%>%
  mutate(Aud_reappoint=case_when(
    Aud_reappoint==TRUE ~ 1,
    is.na(Auditor_name) ~ NA_real_,
    TRUE ~ 0)) # modified here



temp_data<-temp_data %>% group_by(Ownership_group) %>% #codes specific for spec top spec creaton
  mutate(spec_spec=case_when(
    Aud_rotation==1 & Aud_reappoint==1 & lag(GAUDSPEC,n=1L)==1& GAUDSPEC==1 ~1,
    is.na(Aud_reappoint) ~NA_real_,
    TRUE ~ 0 ),
    nonspec_nonspec=case_when(
      Aud_rotation==1 & Aud_reappoint==1 & lag(GAUDSPEC,n=1L)==0&GAUDSPEC==0 ~1,
      is.na(Aud_reappoint) ~NA_real_,
      TRUE ~ 0),
    nonspec_spec=case_when(
      Aud_rotation==1 & Aud_reappoint==1 & lag(GAUDSPEC,n=1L)==0 &GAUDSPEC==1~1,
      is.na(Aud_reappoint) ~NA_real_,
      TRUE ~ 0),
    spec_nonspec=case_when(
      Aud_rotation==1 & Aud_reappoint==1 & lag(GAUDSPEC,n=1L)==1 &GAUDSPEC==0~1,
      is.na(Aud_reappoint) ~NA_real_,
      TRUE~0)) %>% ungroup()


FV<-temp_data %>% select(-Aud_rotation, -Aud_reappoint, -GAUDSPEC) %>% left_join(FV,. )


eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))

rm(temp_data, temp_fn)

# Merging FV to board data from prowess and prime -------------
load("./Prime/Object for merge with FV.Rdata")

FV<-Indep_fem_count %>% select(-3,-4,-5) %>% 
  left_join(FV, .) %>% 
  left_join(.,Dir_Remuneration_Sum) %>% 
  left_join(., Board_Indep_prow) %>% 
  left_join(.,Aud_committee_members) %>% 
  mutate(CEO_dual = case_when(
   is.na(CEO_dual) ~ 0,
    TRUE ~ CEO_dual
  )) %>% ungroup()

remove(Indep_fem_count, Dir_Remuneration_Sum, Board_Indep_prow,Meeting_type_count,Resolution_purpose_count,Aud_committee_members)

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))


# FV RPT Merge ------------------------------------------------------------

load("./Prowess/R Data Prowess/RPT Filtered.Rdata")


RPT_filtered<-RPT_filtered %>% rowwise() %>% 
  mutate(GRPT = sum(c(Total_revenue_expenses_payments, Pay_for_RawM_fingoods,
                      Net_OS_current_receivables_payables, CL,
                      CA, Salary_wage_pay_to_RP,
                      Total_revenue_receipts, Interest_income_from_RP,
                      Pay_for_other_operating_expenses, Total_cap_account_payments,
                      Pay_for_investments, Net_OS_borrowings_taken_or_loan_given,
                      OS_loans_advances_given, Other_transactions,
                      Income_from_services_to_RP, Other_income_from_RP,
                      Income_from_SGRP, Pay_for_marketing_expenses,
                      Total_cap_receipts, FA_sale_receipts,
                      Pay_for_FA_purchase, Pay_for_interest,
                      OS_loans_advances_taken, Pay_for_royalties_tech_fees,
                      Payment_for_other_revenue_expenses, Pay_for_dividend,
                      Investment_sale_receipts, Dividend_income_from_RP,
                      OS_guarantees_given, Rent_paid,
                      OS_close_bal_of_investments, Rent_income_from_RP,
                      Pay_for_processing_charges, Services_not_specified_as_given_received,
                      Guarantees_given_during_year, Guarantees_taken_during_year,
                      Reimbursement_of_expenses_by_RP, SC_issued_during_year,
                      Share_application_money_given_OS_asset, Loans_not_specified_as_given_received,
                      Interest_not_specified_as_given_received, Pay_for_energy_power_fuel,
                      Rent_not_specified_as_given_received, SA_money_received_OS_liab,
                      SA_money_received_during_year, OS_SC,
                      Margin_money_paid_during_year, SA_money_given_during_year,
                      Margin_money_paid_OS_asset, Dividends_not_specified_as_given_received,
                      OS_guarantees_taken, Expenses_reimbursed_to_RP,
                      OS_close_bal_of_FA, Transaction_not_specified,
                      OS_deposits_placed, Doubtful_debt_provisions,
                      Max_amt_payable_RP_during_year, Max_amt_receivable_from_RP_during_year,
                      LoC_given_for_RP, Margin_money_received_during_year,
                      Margin_money_received_OS_liab), na.rm=T)) %>% 
  group_by(Co_name, year) %>% 
  mutate(GRPT = sum(GRPT, na.rm = T)) %>% select(1,4,68) %>% 
  distinct() %>% ungroup()


eeptools::isid(RPT_filtered, vars = c("Co_name", "year"))


FV<-left_join(FV, RPT_filtered) %>% ungroup()

FV<-FV %>% group_by(Ownership_group,year) %>% mutate(group_GRPT=sum(GRPT,na.rm = T)) %>% ungroup()
remove(RPT_filtered)

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))



# FV & Mergers merge ------------------------------------------------------

load("./Prowess/R Data Prowess/Mergers, Acquisitions and Sales List-Unlist.Rdata")

FV<-left_join(FV,Merger_acqui_sales,by=c("Co_name","year")) %>% 
  mutate(across(c(acqui_dum, merger_dum, sales_asset_dum, merger_acqui_dum), ~ case_when(
    is.na(.) ~ 0,
    TRUE ~ .)))


remove(Merger_acqui_sales)

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))


# FV & SNA Merge ----------------------------------------------------------
load("./Prowess/R Data Prowess/SNA.Rdata")


FV<-left_join(FV,SNA)

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))

rm(SNA, T_test, top_50)

# FV filters ----------------------------------------

#From now onwards, we will call FV with 565 vars & 214230 obs as FV_old and newly filtered data as FV
#Saving FV_old for later use or recall

save(FV, file = "./Prowess/R Data Prowess/FV Old.Rdata")




#FV filter by  Industry type
FV<-FV %>% filter(Indust_type==1) %>% 
  filter(!Ownership_group %in% c("Central Govt. - Commercial Enterprises", "State Govt. - Commercial Enterprises", 
                                 "State Bank of India Group", "State and Private sector"))
  #mutate(Log_Age=log(year-Incorp_year))






#Filtering out companies without sales value for at least two years
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






# FV Descriptive (Don't RUN) ----------------------------------------------------------



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





# Accrual M Jones Model ---------------------------------------------------
boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

MJ_model<-function(x){
  lm(Accruals_at ~ One_at + Sales_at_minus_rec_at + Gppe_at , data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, MJ_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, Auditor_name, resid) %>% left_join(FV,.) %>% 
  rename(MJ_resid=resid)
remove(boom, MJ_model) 

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))


# MJ Model ROA ------------------------------------------------------------
boom=FV #assigning FV to new and temporary data object

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

MJ_model_roa<-function(x){
  lm(Accruals_at ~ One_at + Sales_at_minus_rec_at + Gppe_at + Lag_Roa , data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, MJ_model_roa),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, Auditor_name, resid) %>% left_join(FV,.) %>% 
  rename(MJ_roa_resid=resid)
remove(boom, MJ_model_roa) 

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))


MJ_model<-list(
  "MJ_Model" = FV %>% lm(MJ_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "MJ_Model_ROA" = FV %>% lm(MJ_roa_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .))

MJ_model_BG<-list(
  "MJ_Model" = FV %>% filter(Groupstand==1) %>% lm(MJ_resid ~ unlisted+PB+lag(Size_ind_mean, n=1L)+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "MJ_Model_ROA" = FV %>% filter(Groupstand==1) %>% lm(MJ_roa_resid ~ unlisted+PB+lag(Size_ind_mean, n=1L)+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .))




library(modelsummary)
msummary(c(MJ_model), stars = T)
msummary(c(MJ_model_BG), stars = T)






# Roy Prod Model ----------------------------------------------------------


boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

Roy_Prod_model<-function(x){
  lm(Production_at ~ One_at + Sales_at + Chg_Sales_at + Lag_Chg_Sales_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Roy_Prod_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, Auditor_name, resid) %>% left_join(FV,.) %>% 
  rename(Roy_prod_resid=resid)
remove(boom, Roy_Prod_model) 

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))


# Roy CFO Model -----------------------------------------------------------

boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

Roy_CFO_model<-function(x){
  lm(Cfo_at ~ One_at + Sales_at + Chg_Sales_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Roy_CFO_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, Auditor_name, resid) %>% left_join(FV,.) %>% 
  rename(Roy_cfo_resid=resid)
remove(boom, Roy_CFO_model) 

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))



# Roy Disc Model ----------------------------------------------------------
boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004) %>% group_by(NIC_2, year) %>% nest()

Roy_Disc_model<-function(x){
  lm(Disc_expenses_at ~ One_at + Sales_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Roy_Disc_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, Auditor_name, resid) %>% left_join(FV,.) %>%
  rename(Roy_disc_resid=resid)
remove(boom, Roy_Disc_model) 


eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))



Roy_Models<-list(
  "Roy_Prod_Model" = FV %>% lm(Roy_prod_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "Roy_CFO_Model" = FV %>% lm(Roy_cfo_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "Roy_Disc_Model" = FV %>% lm(Roy_disc_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .))

##Regressions for only Business Group companies (Groupstand==1)
Roy_Models_BG<-list(
  "Roy_Prod_Model" = FV %>% filter(Groupstand==1) %>% lm(Roy_prod_resid ~ unlisted+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "Roy_CFO_Model" = FV %>% filter(Groupstand==1) %>% lm(Roy_cfo_resid ~ unlisted+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "Roy_Disc_Model" = FV %>%  filter(Groupstand==1) %>% lm(Roy_disc_resid ~ unlisted+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .))



library(modelsummary)
msummary(c(MJ_model,Roy_Models), stars = T)
msummary(c(MJ_model_BG,Roy_Models_BG), stars = T)


# Vastav Prod Model ----------------------------------------------------------


boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004 & year<=2019 & NIC_2!=45) %>% group_by(NIC_2, year) %>% nest()

Vastav_Prod_model<-function(x){
  lm(Production_at ~ One_at + Sales_at + Chg_Sales_at + Lag_Chg_Sales_at + Size + PB + Lag_Roa + Chg_Sales_at_t1 + Lag_Production_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Vastav_Prod_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))



FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, Auditor_name, resid) %>% left_join(FV,.) %>% 
  rename(Vastav_prod_resid=resid)
remove(boom, Vastav_Prod_model) 

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))


# Vastav CFO Model -----------------------------------------------------------

boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004 & year<=2019 & NIC_2!=45) %>% group_by(NIC_2, year) %>% nest()

Vastav_CFO_model<-function(x){
  lm(Cfo_at ~ One_at + Sales_at + Chg_Sales_at + Lag_Chg_Sales_at + Size + PB + Lag_Roa + Chg_Sales_at_t1 + Lag_Cfo_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Vastav_CFO_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, Auditor_name, resid) %>% left_join(FV,.) %>% 
  rename(Vastav_cfo_resid=resid)
remove(boom, Vastav_CFO_model) 


eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))




# Vastav Disc Model ----------------------------------------------------------
boom=FV

boom<-boom %>% filter(NIC_count>10 & year>=2004 & year<=2019 & NIC_2!=45) %>% group_by(NIC_2, year) %>% nest()

Vastav_Disc_model<-function(x){
  lm(Disc_expenses_at ~ One_at + Sales_at + Chg_Sales_at + Lag_Chg_Sales_at + Size + PB + Lag_Roa + Chg_Sales_at_t1 + Lag_Disc_expenses_at, data=x, na.action = na.exclude)
}

boom<-boom %>% mutate(models= purrr::map(data, Vastav_Disc_model),
                      residuals=purrr::map2(data, models, modelr::add_residuals))


FV<-boom %>% unnest(residuals) %>% select(Co_name, year, NIC_2, Auditor_name, resid) %>% left_join(FV,.) %>%
  rename(Vastav_disc_resid=resid)
remove(boom, Vastav_Disc_model) 


eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))






Vastav_Models<-list(
  "Vastav_Prod_Model" = FV %>% lm(Vastav_prod_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "Vastav_CFO_Model" = FV %>% lm(Vastav_cfo_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "Vastav_Disc_Model" = FV %>% lm(Vastav_disc_resid ~ Groupstand+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .))

Vastav_Models_BG<-list(
  "Vastav_Prod_Model" = FV %>% filter(Groupstand==1) %>% lm(Vastav_prod_resid ~ unlisted+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "Vastav_CFO_Model" = FV %>% filter(Groupstand==1) %>% lm(Vastav_cfo_resid ~ unlisted+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .),
  "Vastav_Disc_Model" = FV %>% filter(Groupstand==1) %>% lm(Vastav_disc_resid ~ unlisted+PB+lag(Size_ind_mean, n=1L)+MJ_roa_resid+Big4+Promoters_percent_shares_held+as.factor(NIC_2)+as.factor(year), data = .))




library(modelsummary)
msummary(c(Roy_Models, MJ_model, Vastav_Models), stars = T)
msummary(c(Roy_Models_BG, MJ_model_BG, Vastav_Models_BG), stars = T)




#FV<-FV %>% select(-starts_with("Roy_"), -starts_with("Vastav"), -starts_with("MJ_"))

remove(MJ_model, MJ_model_BG, Roy_Models, Roy_Models_BG, Vastav_Models, Vastav_Models_BG)


# Centrality Regressions --------------------------------------------------



central_data<-FV %>% select(Closeness, Eigen_centrality, Degree, Closeness_per_year, Eigen_centrality_per_year, Degree_per_year,
                    Closeness_Mkt, Eigen_centrality_Mkt, Degree_Mkt)


Centrality_reg<-lapply(central_data, function(y)
    FV %>% lm(y ~ Aud_rotation + Groupstand + lag(PB) + lag(Size) + lag(Roa) + Big4 + 
                Promoters_percent_shares_held + log(Audit_fees+0.1) + log(Non_Audit_Fee+0.1), data=.))



modelsummary(Centrality_reg, stars = T, statistic = "statistic", 
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             notes = "t statistic in parenthesis",
             title = "Centrality Regression With Auditor Rotation",
             output = "./Prowess/Outputs/Tables/Centrality Regression With Auditor Rotation.html")

             #output = "./Prowess/Outputs/Tables/Centrality Regression With Auditor Rotation.docx")



#centrality regression for Groupstand==1
central_data<-FV %>% filter(Groupstand==1) %>%  select(Closeness, Eigen_centrality, Degree, Closeness_per_year, Eigen_centrality_per_year, Degree_per_year,
                                               Closeness_Mkt, Eigen_centrality_Mkt, Degree_Mkt)




Centrality_reg_BG_only<-lapply(central_data, function(y)
  FV %>% filter(Groupstand==1) %>% lm(y ~ Aud_rotation + lag(PB) + lag(Size) + lag(Roa) + Big4 + 
                                        Promoters_percent_shares_held + log(Audit_fees+0.1) + log(Non_Audit_Fee+0.1), data=.))



modelsummary(Centrality_reg_BG_only, stars = T, statistic = "statistic", 
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             notes = "t statistic in parenthesis",
             title = "Centrality Regression With Auditor Rotation (Only Business Groups)",
             output = "./Prowess/Outputs/Tables/Centrality Regression With Auditor Rotation (Groupstand==1).html")

             #output = "./Prowess/Outputs/Tables/Centrality Regression With Auditor Rotation (Groupstand==1).docx")




rm(Centrality_reg, Centrality_reg_BG_only, indep_vars, central_data)



# REM & Accrual regressions with centrality measures, Auditor rotation and time dummy --------------------------------

FV<- FV %>% mutate(std_prod_roy= scale(Roy_prod_resid), 
                   std_roy_cfo=scale(Roy_cfo_resid),
                   std_roy_disc= scale(Roy_disc_resid),
                   std_prod_vastav= scale(Vastav_prod_resid), 
                   std_cfo_vastav=scale(Vastav_cfo_resid),
                   std_disc_vastav=scale(Vastav_disc_resid))


FV<- FV %>% mutate(Std_rem_roy=(std_prod_roy+std_roy_cfo+std_roy_disc),
                   Std_rem_vastav=(std_prod_vastav+std_cfo_vastav+std_disc_vastav))


FV<- FV %>% mutate(Std_rem_roy2=(abs(Roy_prod_resid)+ abs(Roy_cfo_resid)*-1 + abs(Roy_disc_resid)*-1))

indep_vars<-FV %>% select(Roy_prod_resid, Roy_cfo_resid, Roy_disc_resid)

central_data<-FV %>% select(Closeness, Eigen_centrality, Degree, Closeness_per_year, Eigen_centrality_per_year, Degree_per_year,
                    Closeness_Mkt, Eigen_centrality_Mkt, Degree_Mkt)



Aud_quality_reg<-lapply(indep_vars, function(y)
  lapply(central_data, function(x)
    FV %>% lm(y ~ MJ_roa_resid + Groupstand + lag(PB) + lag(Size) + lag(Roa) + Big4 + 
                Promoters_percent_shares_held + Age + Time_dummy*Aud_rotation + Time_dummy*x, 
              data=., na.action = na.exclude)))


Aud_quality_reg[['MJ_roa_resid']]<-lapply(central_data, function(x)
  FV %>% lm(MJ_roa_resid ~ Std_rem_roy + Groupstand + lag(PB) + lag(Size) + lag(Roa) +Big4 + 
              Promoters_percent_shares_held + Age + 
              Time_dummy*Aud_rotation + Time_dummy*x, data=.))



#Assigning correct coefficient names to regression output

aud_names<-names(Aud_quality_reg[[1]])


indeps<-list(
  c("Constant", "MJ roa residuals", "Groupstand Dummy", "Lag PB", "Lag Size", "Lag RoA", "Big4 Dummy",
    "Percent of Shares held by Promters", "Auditor Rotation", "x", "Age", "Time Dummy", 
    "Auditor Rotation X Time Dummy", "Time Dummy X x" )
)

indeps_mj<-list(
  c("Constant", "Standardized Roy REM Measure", "Groupstand Dummy", "Lag PB", "Lag Size", "Lag RoA", "Big4 Dummy",
    "Percent of Shares held by Promters", "Auditor Rotation", "x", "Age", "Time Dummy", 
    "Auditor Rotation X Time Dummy", "Time Dummy X x"))


x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}

indeps<-map2(aud_names, indeps, x_replacer)

indeps_mj<-map2(aud_names, indeps_mj, x_replacer)

for(x in 1:3){
  for(y in 1:9){
    Aud_quality_reg[[x]][[y]]$coefficients<-set_names(Aud_quality_reg[[x]][[y]]$coefficients, nm=indeps[[y]])
    
  }
}

for(y in 1:9){
  Aud_quality_reg[[4]][[y]]$coefficients<-set_names(Aud_quality_reg[[4]][[y]]$coefficients, nm=indeps_mj[[y]])
}



for(x in 1:4){
  Aud_quality_reg[[x]]<-set_names(Aud_quality_reg[[x]], nm=c(rep(names(Aud_quality_reg)[[x]], 9)))
  
}

rm(indep_vars, x_replacer, aud_names, indeps, indeps_mj, central_data, x , y)


modelsummary(c(Aud_quality_reg[[1]], Aud_quality_reg[[2]], Aud_quality_reg[[3]], Aud_quality_reg[[4]]), stars = T,
             statistic = "statistic", 
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             notes = "t statistic in parenthesis",
             title = "Audit Quality Regression with Centrality and Time Dummy Interactions",
             output = "Audit Quality Regression with Centrality and Time Dummy.html")

#output = "Audit Quality Regression with Centrality and Time Dummy.docx")


rm(Aud_quality_reg)



# REM & Accrual regressions with only Auditor rotation and time dummy --------------------------------


indep_vars<-FV %>% select(Roy_prod_resid, Roy_cfo_resid, Roy_disc_resid)


Aud_quality_reg<-lapply(indep_vars, function(y)
    FV %>% lm(y ~ MJ_roa_resid + Groupstand + lag(PB) + lag(Size) + lag(Roa) + Big4 + 
                Promoters_percent_shares_held + Age + Time_dummy*Aud_rotation, 
              data=., na.action = na.exclude))


Aud_quality_reg[['MJ_roa_resid']]<- FV %>% lm(MJ_roa_resid ~ Std_rem_roy + Groupstand + lag(PB) + lag(Size) + lag(Roa) +Big4 + 
              Promoters_percent_shares_held + Age + 
              Time_dummy*Aud_rotation, data=.)



#Assigning correct coefficient names to regression output

aud_names<-names(Aud_quality_reg[[1]])


indeps<-c("Constant", "MJ roa residuals", "Groupstand Dummy", "Lag PB", "Lag Size", "Lag RoA", "Big4 Dummy",
    "Percent of Shares held by Promters", "Age", "Time Dummy", "Auditor Rotation", 
    "Auditor Rotation X Time Dummy")

indeps_mj<-c("Constant", "Standardized Roy REM Measure", "Groupstand Dummy", "Lag PB", "Lag Size", "Lag RoA", "Big4 Dummy",
    "Percent of Shares held by Promters", "Age", "Time Dummy", "Auditor Rotation", 
    "Auditor Rotation X Time Dummy")


for(x in 1:3){
    Aud_quality_reg[[x]]$coefficients<-set_names(Aud_quality_reg[[x]]$coefficients, nm=indeps)
}


  Aud_quality_reg[[4]]$coefficients<-set_names(Aud_quality_reg[[4]]$coefficients, nm=indeps_mj)



rm(indep_vars,indeps, indeps_mj, x )


modelsummary(Aud_quality_reg, stars = T,
             statistic = "statistic", 
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             notes = "t statistic in parenthesis",
             title = "Audit Quality Regression with only Auditor Rotation and Time Dummy Interactions",
             output = "Audit Quality Regression with only Auditor Rotation and Time Dummy Interactions.html")

#output = "Audit Quality Regression with Centrality and Time Dummy.docx")


rm(Aud_quality_reg)




# REM & Accrual regressions with only centrality measures --------------------------------

indep_vars<-FV %>% select(Roy_prod_resid, Roy_cfo_resid, Roy_disc_resid)

central_data<-FV %>% select(Closeness, Eigen_centrality, Degree, Closeness_per_year, Eigen_centrality_per_year, Degree_per_year,
                    Closeness_Mkt, Eigen_centrality_Mkt, Degree_Mkt)



Aud_quality_reg<-lapply(indep_vars, function(y)
  lapply(central_data, function(x)
    FV %>% lm(y ~ MJ_roa_resid + Groupstand + lag(PB) + lag(Size) + lag(Roa) + Big4 + 
                Promoters_percent_shares_held + Age + Time_dummy*x, 
              data=., na.action = na.exclude)))


Aud_quality_reg[['MJ_roa_resid']]<-lapply(central_data, function(x)
  FV %>% lm(MJ_roa_resid ~ Std_rem_roy + Groupstand + lag(PB) + lag(Size) + lag(Roa) +Big4 + 
              Promoters_percent_shares_held + Age + 
              Time_dummy*x, data=.))



#Assigning correct coefficient names to regression output

aud_names<-names(Aud_quality_reg[[1]])


indeps<-list(
  c("Constant", "MJ roa residuals", "Groupstand Dummy", "Lag PB", "Lag Size", "Lag RoA", "Big4 Dummy",
    "Percent of Shares held by Promters","Age", "Time Dummy", "x", "Time Dummy X x" )
)

indeps_mj<-list(
  c("Constant", "Standardized Roy REM Measure", "Groupstand Dummy", "Lag PB", "Lag Size", "Lag RoA", "Big4 Dummy",
    "Percent of Shares held by Promters", "Age", "Time Dummy", "x", "Time Dummy X x" ))


x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}

indeps<-map2(aud_names, indeps, x_replacer)

indeps_mj<-map2(aud_names, indeps_mj, x_replacer)

for(x in 1:3){
  for(y in 1:9){
    Aud_quality_reg[[x]][[y]]$coefficients<-set_names(Aud_quality_reg[[x]][[y]]$coefficients, nm=indeps[[y]])
    
  }
}

for(y in 1:9){
  Aud_quality_reg[[4]][[y]]$coefficients<-set_names(Aud_quality_reg[[4]][[y]]$coefficients, nm=indeps_mj[[y]])
}



for(x in 1:4){
  Aud_quality_reg[[x]]<-set_names(Aud_quality_reg[[x]], nm=c(rep(names(Aud_quality_reg)[[x]], 9)))
  
}

rm(indep_vars, x_replacer, aud_names, indeps, indeps_mj, central_data, x , y)


modelsummary(c(Aud_quality_reg[[1]], Aud_quality_reg[[2]], Aud_quality_reg[[3]], Aud_quality_reg[[4]]), stars = T,
             statistic = "statistic", 
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             notes = "t statistic in parenthesis",
             title = md("Audit Quality Regression with only Centrality and Time Dummy Interactions"),
             output = "Audit Quality Regression with only Centrality and Time Dummy Interactions.html")

#output = "Audit Quality Regression with Centrality and Time Dummy.docx")


rm(Aud_quality_reg)





# Audit Reappoint Regressions ---------------------------------------------


FV<-FV %>% mutate(GRPT_new=case_when(
  GRPT>=0 ~ GRPT,
  GRPT<0 ~ NA_real_,
  TRUE ~ GRPT
))

#FV<-FV %>% mutate(quant_GRPT = ntile(GRPT, n=10))


indep_vars<-FV %>% select(Roy_prod_resid, Roy_cfo_resid, Roy_disc_resid, MJ_roa_resid)


Aud_reappoint_reg<-lapply(indep_vars, function(x)
  FV %>% glm(Aud_reappoint ~ x + Time_dummy + log(GRPT_new+0.1) + Groupstand + lag(PB) + lag(Size) + lag(Roa) + Big4 + 
               Promoters_percent_shares_held + log(Audit_fees+0.1) + log(Non_Audit_Fee+0.1), 
             data = ., family = "binomial", na.action = na.omit))


indeps<-list(
  c("Constant", "x", "Time Dummy", "Log of RPTs" ,"Groupstand", "Lag of PB", "Lag of Size", "Lag of RoA",  "Big4 Dummy", 
    "Percent of Shares held by Promters", "Log of Audit Fees", "Log of Non-Audit Fees")
)

indeps_mj<-c("Constant", "MJ_roa_resid", "Time Dummy", "Log of RPTs" , "Groupstand", "Lag of PB", "Lag of Size", "Lag of RoA",  "Big4 Dummy", 
             "Percent of Shares held by Promters", "Log of Audit Fees", "Log of Non-Audit Fees")

x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}

aud_names<-names(Aud_reappoint_reg)[1:3]

indeps<-map2(aud_names, indeps, x_replacer)

for(x in 1:3){
  Aud_reappoint_reg[[x]]$coefficients<-set_names(Aud_reappoint_reg[[x]]$coefficients, nm=indeps[[x]])
}


Aud_reappoint_reg[[4]]$coefficients<-set_names(Aud_reappoint_reg[[4]]$coefficients, nm=indeps_mj)

Aud_reappoint_reg<-set_names(Aud_reappoint_reg, nm=rep("Audit Reappointment", times=4))


modelsummary(Aud_reappoint_reg, stars = T,
             exponentiate = TRUE,
             statistic = "statistic", 
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             notes = c("t statistic in parenthesis", "Coefficients are exponentiated to signify Odds Ratio"),
             title = "Audit Reappointment Regression upon audit quality measures",
             output = "Audit Reappointment Regression upon audit quality measures.html")

rm(x_replacer, aud_names, indeps, indeps_mj, indep_vars, x)
rm(Aud_reappoint_reg)

#for only Group companies

indep_vars<-FV %>% filter(Groupstand==1) %>% select(Roy_prod_resid, Roy_cfo_resid, Roy_disc_resid, MJ_roa_resid)


Aud_reappoint_reg<-lapply(indep_vars, function(x)
  FV %>% filter(Groupstand==1) %>% glm(Aud_reappoint ~ x + Time_dummy + log(GRPT_new+0.1) + Groupstand + lag(PB) + lag(Size) + lag(Roa) + Big4 + 
                                         Promoters_percent_shares_held + log(Audit_fees+0.1) + log(Non_Audit_Fee+0.1), 
                                       data = ., family = "binomial"))
indeps<-list(
  c("Constant", "x", "Time Dummy", "Log of RPTs" ,"Groupstand", "Lag of PB", "Lag of Size", "Lag of RoA",  "Big4 Dummy", 
    "Percent of Shares held by Promters", "Log of Audit Fees", "Log of Non-Audit Fees")
)

indeps_mj<-c("Constant", "MJ_roa_resid", "Time Dummy", "Log of RPTs" , "Groupstand", "Lag of PB", "Lag of Size", "Lag of RoA",  "Big4 Dummy", 
             "Percent of Shares held by Promters", "Log of Audit Fees", "Log of Non-Audit Fees")

x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}

aud_names<-names(Aud_reappoint_reg)[1:3]

indeps<-map2(aud_names, indeps, x_replacer)

for(x in 1:3){
  Aud_reappoint_reg[[x]]$coefficients<-set_names(Aud_reappoint_reg[[x]]$coefficients, nm=indeps[[x]])
}


Aud_reappoint_reg[[4]]$coefficients<-set_names(Aud_reappoint_reg[[4]]$coefficients, nm=indeps_mj)

Aud_reappoint_reg<-set_names(Aud_reappoint_reg, nm=rep("Audit Reappointment", times=4))


modelsummary(Aud_reappoint_reg, stars = T,
             exponentiate = TRUE,
             statistic = "statistic", 
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             notes = c("t statistic in parenthesis", "Coefficients are exponentiated to signify Odds Ratio"),
             title = "Audit Reappointment Regression upon audit quality measures (For Group Compnaies)",
             output = "Audit Reappointment Regression upon audit quality measures(For Group Compnaies).html")

rm(model_names, aud_names, indeps, indeps_mj, indep_vars, x)
rm(Aud_reappoint_reg)



# Audit Regressions with for AUDSPEC categories ---------------------------


indep_vars<-FV %>% filter(GAUDSPEC==1) %>% select(Roy_prod_resid, Roy_cfo_resid, Roy_disc_resid, MJ_roa_resid)


Aud_reappoint_reg<-lapply(indep_vars, function(x)
  FV %>% filter(GAUDSPEC==1) %>% glm(Aud_reappoint ~ x*Time_dummy + log(GRPT_new+0.1) + Aud_tenure +  LOSS + currentratio + Invrec_at + debtequity +
                                       EQFIN + Groupstand + lag(PB) + lag(Size) + lag(Roa) + Big4 + merger_acqui_dum + 
                                       Promoters_percent_shares_held*Time_dummy + log(Audit_fees+0.1) + log(Non_Audit_Fee+0.1) + as.factor(NIC_2), 
                                     data = ., family = "binomial", na.action = na.omit))

for(x in 1:4){
  Aud_reappoint_reg[[x]][[1]]<-Aud_reappoint_reg[[x]][[1]][!(names(Aud_reappoint_reg[[x]][[1]]) %in% 
                                                               c(str_extract(names(Aud_reappoint_reg[[x]][[1]]), 
                                                                             pattern = "[:alpha:]*\\.(.*)")))]
}

model_names<-c("Roy Production REM", "Roy CFO REM", "Roy Discretionary REM", "MJ RoA Accruals")

x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}


indeps<-list(
  c("Constant", "x", "Time Dummy", "Log of RPTs", "Auditor Tenure", "LOSS", "Current Ratio", "Invrec AT",
    "Debt Equity", "EQFIN", "Groupstand", "Lag of PB", "Lag of Size", "Lag of RoA",  "Big4 Dummy",
    "Merger and Acquisition Dummy", "Percent of Shares held by Promters", "Log of Audit Fees", 
    "Log of Non-Audit Fees", "x X Time Dummy", "Time Dummy X Percent of Shares held by Promters"))


indeps<-map2(model_names, indeps, x_replacer)

for(x in 1:4){
  Aud_reappoint_reg[[x]]$coefficients<-set_names(Aud_reappoint_reg[[x]]$coefficients, nm=indeps[[x]])
  
}

Aud_reappoint_reg<-set_names(Aud_reappoint_reg, nm=rep("Audit Reappointment", times=4))

rows<-tibble(blah<-c("Industry Fixed Effects"), 
             blue<-c("YES"), 
             blee<-c("YES"),
             glue<-c("YES"),
             clue<-c("YES"))


modelsummary(Aud_reappoint_reg, stars = T,
             exponentiate = TRUE,
             statistic = "statistic", 
             add_rows = rows,
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             notes = c("t statistic in parenthesis", "Coefficients are exponentiated to signify Odds Ratio"),
             title = "Audit Reappointment Regression for specialized auditors upon audit quality measures",
             output = "Audit Reappointment Regression (with GAUDSPEC==1) upon audit quality measures.docx")

rm(x_replacer, model_names, indeps, indep_vars, x, rows)
rm(Aud_reappoint_reg)








#Regressions without REM measures


dep_vars<-FV %>% filter(GAUDSPEC==1) %>% select(Roy_prod_resid, Roy_cfo_resid, Roy_disc_resid, MJ_roa_resid)


Aud_reappoint_reg<-FV %>% filter(GAUDSPEC==1) %>% 
  glm(Aud_reappoint ~ Time_dummy + log(GRPT_new+0.1) + Aud_tenure +  LOSS + currentratio + Invrec_at + debtequity +
        EQFIN + Groupstand + lag(PB) + lag(Size) + lag(Roa) + Big4 + merger_acqui_dum + 
        Promoters_percent_shares_held*Time_dummy + log(Audit_fees+0.1) + log(Non_Audit_Fee+0.1) + as.factor(NIC_2), 
      data = ., family = "binomial", na.action = na.omit)


Aud_reappoint_reg[[1]]<-Aud_reappoint_reg[[1]][!(names(Aud_reappoint_reg[[1]]) %in% 
                                                   c(str_extract(names(Aud_reappoint_reg[[1]]), 
                                                                 pattern = "[:alpha:]*\\.(.*)")))]


indeps<-c("Constant", "Time Dummy", "Log of RPTs", "Auditor Tenure", "LOSS", "Current Ratio", "Invrec AT",
          "Debt Equity", "EQFIN", "Groupstand", "Lag of PB", "Lag of Size", "Lag of RoA",  "Big4 Dummy",
          "Merger and Acquisition Dummy", "Percent of Shares held by Promters", "Log of Audit Fees", 
          "Log of Non-Audit Fees","Time Dummy X Percent of Shares held by Promters")


Aud_reappoint_reg$coefficients<-set_names(Aud_reappoint_reg$coefficients, nm=indeps)

rows<-tibble(col_a<-c("Industry Fixed Effects"), 
             col_b<-c("YES"))



modelsummary(Aud_reappoint_reg, stars = T,
             exponentiate = TRUE,
             statistic = "statistic", 
             add_rows = rows,
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             notes = c("t statistic in parenthesis", "Coefficients are exponentiated to signify Odds Ratio"),
             title = "Audit Reappointment Regression for specialized auditors ",
             output = "Audit Reappointment Regression (with GAUDSPEC==1) without audit quality measures.docx")

rm(indeps, rows, dep_vars)
rm(Aud_reappoint_reg)






# Spec_Spec regressions ---------------------------------------------------

load("./Prowess/R Data Prowess/FV Old.Rdata")

temp_data<-FV %>% filter(Groupstand==1, year>2010 & year<2019) %>% select(spec_spec, spec_nonspec, nonspec_spec, nonspec_nonspec)
temp_data2<-FV %>% filter(Groupstand==1,  year>2010 & year<2019) %>% select(Audit_fees, Non_Audit_Fee)

Aud_reapp_spec<-lapply(temp_data2, function(y)
  lapply(temp_data, function(x)
    FV %>% filter(Groupstand==1,  year>2010 & year<2019) %>% lm(y ~ x+ Aud_tenure +  LOSS + currentratio + Invrec_at + debtequity +
                                          EQFIN + lag(PB) + lag(Size) + lag(Roa) + Big4 + merger_acqui_dum +
                                          Promoters_percent_shares_held, data = ., na.action = na.exclude)))


#renaming iterated indeps

indeps<-list(c("Constant", "x", "Log of RPTs", "Auditor Tenure", "LOSS", "Current Ratio", "Invrec AT",
               "Debt Equity", "EQFIN", "Lag of PB", "Lag of Size", "Lag of RoA",  "Big4 Dummy",
               "Merger and Acquisition Dummy", "Percent of Shares held by Promters"))



x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}

aud_names<-names(Aud_reapp_spec[[1]])

indeps<-map2(aud_names, indeps, x_replacer)

for(j in 1:2){
  for(i in 1:4){
    Aud_reapp_spec[[j]][[i]]$coefficients<-set_names(Aud_reapp_spec[[j]][[i]]$coefficients, nm=indeps[[i]])
  }
}


for(j in 1:2){
  Aud_reapp_spec[[j]]<-set_names(Aud_reapp_spec[[j]], nm=rep(names(Aud_reapp_spec[j]),times=4))
}


msummary(c(Aud_reapp_spec[[1]], Aud_reapp_spec[[2]]),
         stars = T,
         statistic = "statistic", 
         gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
         notes = "t statistic in parenthesis",
         title = "Audit Fee Regression Auditor Specialization and Time Dummy Interactions")
         output = "Audit Fee Regression Auditor Specialization and Time Dummy Interactions.html")


rm(temp_data, temp_data2, x_replacer, FV, aud_names, indeps, j, i, Aud_reapp_spec)


#with REM measures

load("./Prowess/R Data Prowess/Prowess List-Unlist Data.Rdata")

temp_data<-FV %>% filter(Groupstand==1) %>% select(spec_spec, spec_nonspec, nonspec_spec, nonspec_nonspec)
temp_data2<-FV %>% filter(Groupstand==1) %>% select(Audit_fees, Non_Audit_Fee)

Aud_reapp_spec<-lapply(temp_data2, function(y)
  lapply(temp_data, function(x)
    FV %>% filter(Groupstand==1) %>% lm(y ~ x*Time_dummy + x*log(GRPT+0.1)+ x*Roy_prod_resid+ x*Roy_cfo_resid + x*Roy_disc_resid + x*MJ_roa_resid + Aud_tenure +  LOSS + currentratio + Invrec_at + debtequity +
                                          EQFIN + lag(PB) + lag(Size) + lag(Roa) + Big4 + merger_acqui_dum +
                                          Promoters_percent_shares_held, data = ., na.action = na.exclude)))


#renaming iterated indeps

indeps<-list(c("Constant", "x", "Time Dummy", "Log of RPTs","Roy Production REM", "Roy CFO REM", "Roy Discretionary REM", "MJ RoA Accruals", 
               "Auditor Tenure", "LOSS", "Current Ratio", "Invrec AT",
               "Debt Equity", "EQFIN", "Lag of PB", "Lag of Size", "Lag of RoA",  "Big4 Dummy",
               "Merger and Acquisition Dummy", "Percent of Shares held by Promters", "x X TimeDummy", "x X Log RPTs",
               "x X Roy Production REM", "x X Roy CFO REM", "x X Roy Discretionary REM", "x X MJ RoA Accruals"))



x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}

aud_names<-names(Aud_reapp_spec[[1]])

indeps<-map2(aud_names, indeps, x_replacer)

for(j in 1:2){
  for(i in 1:4){
    Aud_reapp_spec[[j]][[i]]$coefficients<-set_names(Aud_reapp_spec[[j]][[i]]$coefficients, nm=indeps[[i]])
  }
}


for(j in 1:2){
  Aud_reapp_spec[[j]]<-set_names(Aud_reapp_spec[[j]], nm=rep(names(Aud_reapp_spec[j]),times=4))
}


msummary(c(Aud_reapp_spec[[1]], Aud_reapp_spec[[2]]),
         stars = T,
         statistic = "statistic", 
         gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
         notes = "t statistic in parenthesis",
         title = "Audit Fee Regression Auditor Specialization, Time Dummy and REM Interactions",
         output = "Audit Fee Regression Auditor Specialization, Time Dummy and REM Interactions.html")


rm(temp_data, temp_data2, x_replacer, aud_names, indeps, j, i, Aud_reapp_spec)



 
  
  
# Saving Image ------------------------------------------------------------
save.image("./Prowess/R Data Prowess/Prowess List-Unlist Data.Rdata")





