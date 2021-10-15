library(tidyverse)


# AGM EGM -----------------------------------------------------------------
AGM_EGM<-readxl::read_excel("./Prime/Primedata/AGM EGM Data.xlsx", sheet="AGM EGM clean")


AGM_EGM<-AGM_EGM %>% rename(Co_name =`Company Name`,
                             NSE_symbol = `NSE Symbol`,
                   Type_of_meeting = `Type (AGM/EGM/CCM)`,
                   Date_of_event= `Date of Event`,
                   Time_of_event= `Time of Event`,
                   Notice_date = `Notice Date`,
                   Location_of_event = `Location of Event`,
                   Book_closure_start_date = `Book Closure Start Date`,
                   Book_closure_end_date = `Book Closure End Date`,
                   Record_date_for_AGM_EGM = `Record Date for the Purpose of AGM/EGM`,
                   Physical_voting_deadline = `Deadline for Physical Voting`,
                   Physical_voting_deadline_time = `Deadline for Physical Voting-Time`,
                   Physical_proxy_cutoff_date = `Physical Proxy Cut-off Date (Last Date for Receiving Proxy Form)`,
                   Physical_proxy_cutoff_time = `Physical Proxy Cut-off Date (Time for Receiving Proxy Form)`,
                   Evoting_start_date = `E-Voting Start Date`,
                   Evoting_start_time = `E-Voting Start Time`,
                   Evoting_end_date = `E-Voting End Date`,
                   Evoting_end_time = `E-Voting End Time`,
                   Evoting_event_number = `E-Voting Event No.`,
                   Evoting_cutoff_date = `E-Voting Cutoff Date/Record Date for E-Voting`,
                   Evoting_cutoff_time = `E-Voting Cutoff Time/Record Time for E-Voting`,
                   SH_num_on_Evoting_cutoff_date = `No. of Shareholders as on E-Voting Cut off Date`,
                   Total_num_SH_on_record_date = `Total No. of Shareholders on Record Date`,
                   SH_present_inperson_PPG = `SH present In Person/ Proxy - Promoter & Promoter Group`,
                   SH_present_inperson_Public = `SH present In Person/ Proxy - Public`,
                   SH_present_inperson_Total = `SH present In Person/ Proxy - Total`,
                   SH_present_video_PPG = `SH present Video Conference - Promoter & Promoter Group`,
                   SH_present_video_Public = `SH present Video Conference - Public`,
                   SH_present_video_Total = `SH present Video Conference - Total`,
                   Financial_year = `Financial Year`,
                   Cancel_Reschedule = `Cancelled(C) /Rescheduled (R )`) %>% 
  mutate(Type_of_meeting= as.factor(Type_of_meeting),
                            Agency= as.factor(Agency))

#Co_list<-AGM_EGM %>% distinct(Co_name)

#eeptools::isid(AGM_EGM_Resolutions, vars = c("cntrl_no","Serial_num"))


# AGM EGM Resolutions -----------------------------------------------------

AGM_EGM_Resolutions<-readxl::read_excel("./Prime/Primedata/AGM EGM Data.xlsx", sheet="AGM EGM RESOLUTIONS Clean")

AGM_EGM_Resolutions<- AGM_EGM_Resolutions %>% rename(Co_name =`Company Name`,
                               NSE_symbol = `NSE Symbol`,
                               Type_of_meeting = `Type (AGM/EGM)`,
                               Date_of_event= `Date of Event`, 
                               Serial_num = S.No.,                               
                               Resolution_purpose = `Resolution/ Purpose`,
                               Record_date = `Record Date`,
                               Ordinary_special = `Ordinary/Special`,
                               Mgt_SH_proposed = `Mangement Shareholder Proposed`,
                               Mode_of_voting = `Mode of Voting (Show of Hands, Postal Ballot, E-Voting, Poll)`,
                               TPP_votes_cast_shares_held_percent = `TPP No. of Votes Cast as % of Total Shares Held`,
                               TPP_votes_infavour_num_valid_votes = `TPP No. of Votes in favour of Resolution as % of Total No. of Valid Votes`,
                               TPI_votes_cast_shares_held_percent = `TPI No. of Votes Cast as % of Total Shares Held`,
                               TPI_votes_infavour_num_valid_votes = `TPI No. of Votes in favour of Resolution as % of Total No. of Valid Votes`, 
                               TPO_votes_cast_shares_held_percent = `TPO No. of Votes Cast as % of Total Shares Held`,
                               TPO_votes_infavour_num_valid_votes = `TPO No. of Votes in favour of Resolution as % of Total No. of Valid Votes`, 
                               Total_votes_cast_shares_held_percent = `Total No. of Votes Cast as % of Total Shares Held`,
                               Total_votes_infavour_percent_valid_votes = `Total No. of Votes in favour of Resolution as % of Total No. of Valid Votes`,
                               Pass_fail = `Resolution Passed/ Failed`) %>% 
  mutate(Type_of_meeting = as.factor(Type_of_meeting),
         Resolution_purpose = as.factor(Resolution_purpose),
         Ordinary_special = as.factor(Ordinary_special),
         Mgt_SH_proposed = as.factor(Mgt_SH_proposed),
         Mode_of_voting = as.factor(Mode_of_voting),
         Pass_fail = as.factor(Pass_fail),
         RPT = as.factor(RPT)) %>% 
  mutate(Serial_num = case_when( #To remove for a single error in Serial_num
           NSE_symbol=="SSINFRA" & TPP_votes_cast_shares_held_percent==49.76 ~ 3,
           TRUE ~ Serial_num
         ))
#eeptools::isid(AGM_EGM_Resolutions, vars = c("cntrl_no","Serial_num"))


# Indian Boards -----------------------------------------------------------
Director_data<-readxl::read_excel("./Prime/Primedata/Indian_Boards_Data_of_all_NSE-listed_Companies_17.02.2021.xlsx", sheet = "MasterProfile Clean")

Director_data<-Director_data %>% rename(Co_name = Company,
  Director_salutation = `Director Salutation`,
       Director_first_name = `Director First Name`,
       Director_middle_name = `Director Middle Name`,
       Director_surname = `Director Surname`,
       DoB = `Date of Birth`,
       Appointment_date = `Appointment Date`,
       Civil_service_member = `Member of Civil Services`,
       Promoter_director = `Promoter Director (Yes/No)`,
       Position = `Position on Board`,
       Independent = `Independent (Yes/No)`,
       Cessation_date = `Cessation Date`,
       Cessation_reason = `Cessation Reason`,
       Other_directorship_1 = `Other Directorship 1`,
       Other_directorship_2 = `Other Directorship 2`,
       Other_directorship_3 = `Other Directorship 3`,
       Other_directorship_4 = `Other Directorship 4`,
       Other_directorship_5 = `Other Directorship 5`,
       Other_directorship_6 = `Other Directorship 6`,
       Other_directorship_7 = `Other Directorship 7`,
       Other_directorship_8 = `Other Directorship 8`,
       Other_directorship_9 = `Other Directorship 9`,
       Other_directorship_10 = `Other Directorship 10`,
       Other_directorship_11 = `Other Directorship 11`,
       Other_directorship_12 = `Other Directorship 12`,
       Other_directorship_13 = `Other Directorship 13`,
       Other_directorship_14 = `Other Directorship 14`,
       Other_directorship_15 = `Other Directorship 15`,
       Brief_profile = `Brief Profile`) %>% select(-43) %>% 
  mutate(DoB = lubridate::dmy(DoB),
    Female = case_when(
      is.na(Gender) ~ NA_real_,
      Gender == "M" ~ 0,
      Gender == "F" ~ 1,
      TRUE ~ 3),
    Appointment_date = lubridate::dmy(Appointment_date),
    Civil_service_member = Civil_service_member,
    Promoter_director = case_when(
      is.na(Promoter_director) ~ NA_real_,
      Promoter_director == "YES" ~ 1, 
      Promoter_director == "NO" ~ 0,
      TRUE ~ 99),
    Position = forcats::as_factor(Position),
    Independent = case_when(
      is.na(Independent) ~ NA_real_,
      Independent == "YES" ~ 1,
      Independent == "NO" ~ 0,
      TRUE ~ 99),
    Cessation_date = lubridate::dmy(Cessation_date))
  

Committee_data<-readxl::read_excel("./Prime/Primedata/Indian_Boards_Data_of_all_NSE-listed_Companies_17.02.2021.xlsx", sheet ="Committee Details Clean") %>% 
  rename(Co_name = Company,
         Director_salutation = `Director Salutation`,
         Director_first_name = `Director First Name`,
         Director_middle_name = `Director Middle Name`,
         Director_surname = `Director Surname`,
         Committee_name = `Committee Name`,
         Joining_date = `Date of Joining`) %>% 
  mutate(Joining_date = lubridate::dmy(Joining_date))
  

# Remuneration Cleaning ---------------------------------------------------

Director_comm_5<-readxl::read_excel("./Prime/Primedata/IndianBoardsData/Indian Boards Data as on 31-Mar-2013.xlsx", sheet = "Committee Details") %>% mutate(year=2013)
Director_comm_6<-readxl::read_excel("./Prime/Primedata/IndianBoardsData/Indian Boards Data as on 31-Mar-2014.xlsx", sheet = "Committee Details") %>% mutate(year=2014)
Director_comm_7<-readxl::read_excel("./Prime/Primedata/IndianBoardsData/Indian Boards Data as on 31-Mar-2015.xlsx", sheet = "Committee Details") %>% mutate(year=2015)
Director_comm_8<-readxl::read_excel("./Prime/Primedata/IndianBoardsData/Indian Boards Data as on 31-Mar-2016.xlsx", sheet = "Committee Details") %>% mutate(year=2016)
Director_comm_9<-readxl::read_excel("./Prime/Primedata/IndianBoardsData/Indian Boards Data as on 31-Mar-2017.xlsx", sheet = "Committee Details") %>% mutate(year=2017)
Director_comm_10<-readxl::read_excel("./Prime/Primedata/IndianBoardsData/Indian Boards Data as on 31-Mar-2018.xlsx", sheet = "Committee Details") %>% mutate(year=2018)
Director_comm_11<-readxl::read_excel("./Prime/Primedata/IndianBoardsData/Indian Boards Data as on 31-Mar-2019.xlsx", sheet = "Committee Details") %>% mutate(year=2019)
Director_comm_12<-readxl::read_excel("./Prime/Primedata/IndianBoardsData/Indian Boards Data as on 31-Mar-2020.xlsx", sheet = "Committee Details") %>% mutate(year=2020)

Committee_details<-bind_rows(Director_comm_5, Director_comm_6, Director_comm_7, Director_comm_8, Director_comm_9,
                             Director_comm_10, Director_comm_11, Director_comm_12) %>% 
  rename(Co_name = Company,
         Director_salutation = `Director Salutation`,
         Director_first_name = `Director First Name`,
         Director_middle_name = `Director Middle Name`,
         Director_surname = `Director Surname`,
         Committee_name = `Committee Name`,
         Joining_date = `Date of Joining`,
         Person_Code = `Person Code`) %>% 
  mutate(Joining_date = lubridate::dmy(Joining_date),
         Director_name = paste0(Director_first_name, " ", Director_middle_name, " ", Director_surname)) %>% 
  select(-Director_first_name, -Director_middle_name, -Director_surname, -Director_salutation)


remove(Director_comm_5, Director_comm_6, Director_comm_7, Director_comm_8, Director_comm_9,
Director_comm_10, Director_comm_11, Director_comm_12)
       
       
#eeptools::isid(Committee_details, vars = c("Co_name", "year", "Person_Code", "Committee_name"))
#same guy with same position but different joining date. let go of eeptools



# Postal Ballots  ---------------------------------------------------------
Postal_ballots<-readxl::read_excel("./Prime/Primedata/Postal Ballots Data.xlsx", sheet = "POSTAL BALLOT Clean")

Postal_ballots<-Postal_ballots %>% rename(Co_name =`Company Name`,
                          NSE_symbol = `NSE Symbol`,
                          Post_ballot_date_declaration =`Date of Declaration of Postal Ballot`,
                          Post_ballot_open_date = `Opening Date of Postal Ballot`,
                          Post_ballot_open_time = `Opening Time of Postal Ballot`,
                          Post_ballot_close_date = `Closing Date of Postal Ballot`,
                          Post_ballot_close_time = `Closing Time of Postal Ballot`,
                          Post_ballot_outcome_declare_date = `Date of Declaration of Outcome of Postal Ballot`,
                          Post_ballot_outcome_declare_time = `Time of Declaration of Outcome of Postal Ballot`,
                          Record_date = `Record Date`,
                          Cancel_Reschedule = `Cancelled(C) /Rescheduled®` ,
                          Evoting_start_date = `E-Voting Start Date`,
                          Evoting_start_time = `E-Voting Start Time`,
                          Evoting_end_date = `E-Voting End Date`,
                          Evoting_end_time = `E-Voting End Time`,
                          Evoting_event_number = `E-Voting Event No.`,
                          Evoting_cutoff_date = `E-Voting Cutoff Date/Record Date for E-Voting`,
                          Evoting_cutoff_time = `E-Voting Cutoff Time/Record Time for E-Voting`,
                          SH_num_on_Evoting_cutoff_date = `No. of Shareholders as on E-Voting Cut off Date`,
                          Total_num_SH_on_record_date = `Total No. of Shareholders on Record Date`,
                          Scrutinizer = `Scrutinizer Name/s`) %>%  select(-25:-35)














# Board Data Merging ------------------------------------------------------
#load("E://MCA/Prime/Prime Data.Rdata")
load("E://MCA/Prime/Cleaned Rdata/Postal_Ballot_Data_Resolutions.Rdata")
load("E://MCA/Prowess/Cleaned Rdata/board_data.Rdata")

AGM<-left_join(AGM_EGM_Resolutions, AGM_EGM, by=c("Co_name","NSE_symbol","Industry","cntrl_no","Type_of_meeting", "Date_of_event"))



# Director Data Merging ---------------------------------------------------

load("./Prime/Cleaned Rdata/Director_data.Rdata")
load("./Prime/Cleaned Rdata/Director_remuneration.Rdata")


Director_data_remuneration<-Director_data_remuneration %>% mutate(year=as.numeric(stringr::str_sub(year, end = -4L)))

Prime_Director<-left_join(Director_data, Director_data_remuneration) %>% 
  left_join(., Committee_details, by = c("Co_name", "Person_Code", "year", "Position", "Director_name"))

blah<-Prime_Director %>% group_by(Co_name, year) %>%  count(year) %>% rename(Director_count = n) 
Prime_Director<-left_join(Prime_Director, blah)
  
  
  bluh<-Prime_Director %>% arrange(Co_name, Director_name, year) %>% 
    select(Co_name, Person_Code, year, Independent)
  
  bluh<-Prime_Director %>% mutate(indep_NA = case_when(
    is.na(Independent) ~ 1,
    TRUE ~ 99)) %>% select(Co_name, Person_Code, year, Independent, indep_NA)
  
# Saving Image ------------------------------------------------------------
save.image("./Prime/Prime Data.Rdata")

