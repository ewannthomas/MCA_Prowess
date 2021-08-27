library(tidyverse)

# Profit 1 Cleaning ---------------------------------------------------------
Profit_1<- readxl::read_excel("./Prowess/Profit/Profit 1.xlsx")


Profit_1 <- Profit_1 %>% pivot_longer(.,2:(ncol(Profit_1)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Profit_after_tax = 'Profit after tax',
         PBDITA  = 'PBDITA',
         PBPT = 'PBPT',
         PBT = 'PBT',
         PBIT_net = 'PBIT net of P&E&OI&FI',
         PBIT = 'PBIT',
         Cash_profit = 'Cash profit',
         PAT = 'PAT net of P&E',
         Cash_profit_net_PE = 'Cash profit net of P&E',
         OP_profit_NFC = 'Operating  profit of non-financial companies',
         OP_profit_FC = 'Operating profit of financial companies')


# Profit 2 Cleaning ---------------------------------------------------------
Profit_2<- readxl::read_excel("./Prowess/Profit/Profit 2.xlsx")

Profit_2<- Profit_2 %>% pivot_longer(.,2:(ncol(Profit_2)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company name`,
         PBPT_net_fin = 'PBPT net of P&E&OI to inc fin serv',
         PBPT_net_emp = 'PBPT net of P&E&OI per employee',
         PAT_continuing = 'PAT from continuing ops as % of income from continuing ops',
         PAT_discontinuing = 'PAT discont ops as % of income from disocont ops',
         PAT_from_continuing = 'Profit after tax from continuing operations',
         P_L_on_discontinuing = 'Profit/loss after tax on discontinuing operations',
         NPBT_Extra_Ordinary = 'Net profit before tax and extra ordinary items')

Profit= left_join(Profit_1, Profit_2, by= c("Co_name","year")) 

remove(Profit_1, Profit_2)

# Income 1 Cleaning -------------------------------------------------------
Income_1<- readxl::read_excel("./Prowess/Income/Income 1.xlsx")        

Income_1 <- Income_1 %>% pivot_longer(.,2:(ncol(Income_1)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,        
         TI = 'Total income',
         Sales =  'Sales',
         Income_Financial =  'Income from financial services',
         Other_Income = 'Other income',
         Industrial_sales = 'Industrial sales',
         Income_non_fin = 'Income from non financial services',
         Sales_returns = 'Sales returns',
         Trade_discount = 'Trade discount',
         FBFSI = 'Fee based financial services income',
         FundBFSI = 'Fund based financial services income',
         Other_fiancial = 'Other financial services income',
         Expenses_recovered = 'Expenses recovered',
         LDCR = 'Liquidated damages and claims received',
         Amortaisation = 'Amortisation of deferred income',
         Government_Grant = 'Revenue government grant',
         Miscellaneous = 'Miscellaneous income' )



# Income 2 Cleaning -------------------------------------------------------
Income_2<- readxl::read_excel("./Prowess/Income/Income 2.xlsx")

Income_2 <- Income_2 %>% pivot_longer(.,2:(ncol(Income_2)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         PP_Extra_Ordinary = 'Prior period and extra ordinary income',
         Prior_period	= 'Prior period income',
         Extra_ordinary	= 'Extra ordinary income',
         Capitalised	= 'Income capitalised',
         Interest_Capitalised	= 'Interest income capitalised',
         DRE = 'Income transferred to DRE',
         TDS	= 'Tax deducted at source (TDS)',
         Internal_Transfers	= 'Internal transfers',
         TI_Extra_Ordinary	= 'Total income net of Prior period and extra ordinary income',
         Sales_CIS = 'Sales and change in stocks',
         Net_Sales = 'Net sales',
         Sales_NFA = 'Sales Net fixed assets')




Income= left_join(Income_1, Income_2, by= c("Co_name","year"))

remove(Income_1, Income_2)




# Stock Price and Capital Changes -----------------------------------------
Adjust_close_price<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Adjusted Closing Price.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Adj_closing_price") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -5L))) %>% rename(Co_name= `Company Name`)

Cash_EPS<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Cash EPS.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Cash_EPS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -5L))) %>% rename(Co_name= `Company Name`)

Cons_cash_EPS<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Consolidated Cash EPS.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Cons_cash_EPS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Cons_EPS<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Consolidated EPS.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Cons_EPS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

EPS<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/EPS and PB.xlsx") %>% 
  select(1,starts_with("EPS")) %>% 
  tidyr::pivot_longer(starts_with("EPS"), names_to = "year", values_to = "EPS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

PB<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/EPS and PB.xlsx") %>% 
  select(1,starts_with("P/B")) %>% 
  tidyr::pivot_longer(starts_with("P/B"), names_to = "year", values_to = "PB") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Face_value<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Face Value.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Face_value") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Mkt_capital<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Market Capitalisation.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Mkt_capital") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

PE<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/P E.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "P_E") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Shares_OS<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Shares outstatnding.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Shares_OS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

BV_per_share<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("BV per Share")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "BV_per_share") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)


Yield<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Yield")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Yield") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Turnover<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Turnover")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Turnover") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Shares_traded<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Shares traded")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Shares_traded") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Weighted_avg_price<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Weighted")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Weighted_avg_price") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Transactions_num<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Number of Transactions")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Transactions_num") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Shares_deliverable<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Shares deliverable2")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Shares_deliverable") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Shares_deliverable_asp_traded<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Shares deliverable as")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Shares_deliverable_asp_traded") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

EV<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Enterprise value2")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "EV") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Mkt_capital_by_EV<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Market Capitalisation")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Mkt_capital_by_EV") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

EV_by_PBDITA<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Stock Prices & Capital Changes NSE 1.xlsx") %>% 
  select(1,starts_with("Enterprise Value / PBDITA")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "EV_by_PBDITA") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Total_returns<-readxl::read_excel("./Prowess/Stock Prices & Capital Changes/Total Returns.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Total_returns") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Stock_prices_capital_changes<-left_join(BV_per_share, Adjust_close_price, by=c("Co_name", "year")) %>% 
  left_join(.,Cash_EPS, by=c("Co_name", "year")) %>%
  left_join(.,Cons_cash_EPS, by=c("Co_name", "year")) %>%
  left_join(.,Cons_EPS, by=c("Co_name", "year")) %>%
  left_join(.,EPS, by=c("Co_name", "year")) %>%
  left_join(.,PB, by=c("Co_name", "year")) %>%
  left_join(.,Face_value, by=c("Co_name", "year")) %>% 
  left_join(.,Mkt_capital, by=c("Co_name", "year")) %>%
  left_join(.,PE, by=c("Co_name", "year")) %>%
  left_join(.,Shares_OS, by=c("Co_name", "year")) %>%
  left_join(.,Yield, by=c("Co_name", "year")) %>%
  left_join(.,Turnover, by=c("Co_name", "year")) %>%
  left_join(.,Shares_traded, by=c("Co_name", "year")) %>%
  left_join(.,Weighted_avg_price, by=c("Co_name", "year")) %>%
  left_join(.,Transactions_num, by=c("Co_name", "year")) %>% 
  left_join(.,Shares_deliverable, by=c("Co_name", "year")) %>% 
  left_join(.,Shares_deliverable_asp_traded, by=c("Co_name", "year")) %>% 
  left_join(.,EV, by=c("Co_name", "year")) %>% 
  left_join(.,Mkt_capital_by_EV, by=c("Co_name", "year")) %>% 
  left_join(.,EV_by_PBDITA, by=c("Co_name", "year")) %>% 
  left_join(.,Total_returns, by=c("Co_name", "year"))

remove(EPS, PB, Cons_EPS, Cons_cash_EPS, Cash_EPS, BV_per_share, Adjust_close_price, Face_value, Mkt_capital, PE, Shares_OS, Yield, Shares_traded, Turnover, Weighted_avg_price, Transactions_num,
       Shares_deliverable, Shares_deliverable_asp_traded, EV, Mkt_capital_by_EV, EV_by_PBDITA, Total_returns)

#eeptools::isid(Stock_prices_capital_changes, vars = c("Co_name", "year"))


# Merging Income, Profit, Stock Price and Capital Changes -----------------

load("~/MCA/Prowess/Income/Income and Profit.Rdata")  #Load these cleaned R mothers to merge the objects
load("~/MCA/Prowess/Income/Income and Profit.Rdata")

#eeptools::isid(FV_1, vars = c("Co_name", "year"))

FV_1<-left_join(Profit, Income, by= c("Co_name", "year")) %>% 
  left_join(., Stock_prices_capital_changes, by= c("Co_name", "year"))

remove(Income, Profit, Stock_prices_capital_changes)





# Change in Stock  ------------------------------------------------

ChangeinStock<- readxl::read_excel("./Prowess/ChangeinStock/ChangeinStock.xlsx")

ChangeinStock <- ChangeinStock %>% pivot_longer(.,2:ncol(ChangeinStock), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         ChgStock=`Change in stock`,
         ChgStock_fingoods=`Change in stock of finished goods`,
         OpgStock_fingoods=`Opening stock of finished goods`,
         ClgStock_fingoods=`Closing stock of finished goods`,
         ChgStock_wip_semifingoods=`Change in stock of wip and semifinished goods`,
         OpgStock_wip_semifingoods=`Opening stock of wip and semifinished goods`,
         ClgStock_wip_semifingoods=`Closing stock of wip and semifinished goods`,
         StockIncr=`Increase in stock due to change in valuation`,
         StockDecr=`Decrease in stock due to change in valuation`)

# Contingent Liabilities --------------------------------------------------

# Liabilities

CntgtLiabilities<- readxl::read_excel("./Prowess/ContingentLiabilities/Liabilities.xlsx")

CntgtLiabilities <- CntgtLiabilities %>% pivot_longer(.,2:ncol(CntgtLiabilities), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         CntgtLiab=`Contingent liabilities`,
         Bills_cheques_disc=`Bills and cheques discounted`,
         Accept_endorse_oblg=`Acceptances, endorsement obligation (banks)`,
         Credit_letter_comp=`Letter of credit issued by the company`,
         Credit_letter_compforgrp=`Letter of credit issued by the company for group companies`,
         Credit_letter_banks=`Letter of credit issued by banks`,
         Disputed_taxes=`Disputed taxes`,
         Disputed_incometax=`Disputed income tax`,
         Disputed_excise=`Disputed excise`,
         Disputed_customduty=`Disputed custom duties`,
         Disputed_salestax=`Disputed sales tax`,
         Disputed_othertax=`Others disputed taxes including octroi and local taxes`)


# Guarantee

Guarantee <- readxl::read_excel("./Prowess/ContingentLiabilities/Guarantee.xlsx")

Guarantee <- Guarantee %>% pivot_longer(.,2:ncol(Guarantee), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Dispute_claims=`Disputed claims or others`,
         Dispute_licensefees=`Disputed licence fees`,
         Dispute_leaserentals=`Disputed lease rentals`,
         Otherclaimsdisputed=`Other claims disputed`,
         Gnt_n_cntrguarantee=`Guarantees and counter-guarantees`,
         Gnt=`Guarantees`,
         Guarantee_grpcomp=`Guarantee for group companies`,
         Guarantee_in_India=`Guarantee given in India (for finance companies)`,
         Guarantee_outside=`Guarantee given outside India (for finance companies)`,
         Cntrguarantee_bycomp=`Counter guarantees by company`,
         Cntrguarantee_forgrpcomp=`Counter guarantees for group companies`)


# Committments
Committments <- readxl::read_excel("./Prowess/ContingentLiabilities/othercommittments.xlsx")

Committments <- Committments %>% pivot_longer(.,2:ncol(Committments), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         CntgtLiabOther=`Other contingent liabilities`,
         Arrs_dividend=`Arrears of preference dividend`,
         Empdues=`Unprovided employee dues`,
         Liab_paidupshares=`Liabilities of un-called and partly paidup shares & debentures`,
         Liab_underwrtoblg=`Liabilities of underwriting obligation`,
         CntgtLiabMisc=`Other miscellaneous contingent liabilities`,
         Rnd_expensestot=`Research & development expenses (capital & current account)`,
         Rnd_expensescapacct=`Research & development expenses - capital account`,
         Rnd_expensescurracct=`Research & development expenses - current account`,
         Cmmts=`Commitments`,
         Cmmts_on_capacct=`Commitment on capital account`,
         Cmmts_on_revacct=`Commitment on other/revenue account`)

CntgtLiabilities<-left_join(CntgtLiabilities, Guarantee, by= c("Co_name","year")) %>%
  left_join(.,Committments, by=c("Co_name","year"))

remove(Guarantee,Committments)

# CSR_Disclosures ---------------------------------------------------------

CSR_Disclosures<- readxl::read_excel("./Prowess/CSR_Disclosures/CSR_Disclosures.xlsx")

CSR_Disclosures <- CSR_Disclosures %>% pivot_longer(.,2:ncol(CSR_Disclosures), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Avg_netprofit=`Average net profit for last three financial year`,
         CSR_expend_incurred=`CSR expenditure to be incurred as per Companies Act 2013`,
         Tot_amtspent_CSR=`Total amount spent on CSR activities during the year`,
         Tot_amtspent_CSR_curryear=`Of which: Amount spent on CSR actvities for the current year`,
         Tot_amtunspent_CSR=`Total CSR amount unspent as on year end`,
         Tot_amtunspent_CSR_curryear=`CSR amount unspent pertaining to the current year`)

# Expenses ----------------------------------------------------------
Expenses1_10<- readxl::read_excel("./Prowess/TotalExpenses/Expenses1-10.xlsx")

Expenses1_10 <- Expenses1_10 %>% pivot_longer(.,2:ncol(Expenses1_10), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Raw_materials=`Raw materials, stores & spares`,
         Pkg_expenses=`Packaging and packing expenses`,
         Purchase_fingoods=`Purchase of finished goods`,
         Power_fuel_water_chgs=`Power, fuel & water charges`,
         Comps_Emp=`Compensation to employees`,
         Indirect_taxes=`Indirect taxes`,
         Royalitiesnother_fees=`Royalties, technical know-how fees, etc`,
         Rent_lease=`Rent & lease rent`,
         Repairs_maint=`Repairs & maintenance`,
         Ins_prem_paid=`Insurance premium paid`)

####

Expenses11_20<- readxl::read_excel("./Prowess/TotalExpenses/Expenses11-20.xlsx")

Expenses11_20 <- Expenses11_20 %>% pivot_longer(.,2:ncol(Expenses11_20), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Outsrc_mfg_jobs=`Outsourced manufacturing jobs`,
         Outsrc_prof_jobs=`Outsourced professional jobs`,
         NonExecdir_fees=`Non-executive directors' fees`,
         Sellndistr_expenses=`Selling & distribution expenses`,
         Trvl_expenses=`Travel expenses`,
         Comm_expenses=`Communications expenses`,
         PrinnStat_expenses=`Printing & stationery expenses`,
         Misc_expend=`Miscellaneous expenditure`,
         Otherexpenses_indust=`Other operational expenses of industrial enterprises`,
         Otherexpenses_nonfin=`Other operational expenses of non-financial services enterprises`)

##

Expenses21_32<- readxl::read_excel("./Prowess/TotalExpenses/Expenses21_32.xlsx")

Expenses21_32 <- Expenses21_32 %>% pivot_longer(.,2:ncol(Expenses21_32), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Tot_expenses=`Total expenses`,
         Fin_services=`Financial services expenses`,
         Provns=`Provisions`,
         Deprc=`Depreciation / Amortisation (net of transfer from revaluation reserves)`,
         Amort=`Amortisation`,
         Write_offs=`Write-offs`,
         Other_capital=`Other capitalisation`,
         Expenses_to_DRE=`Other expenses transferred to DRE`,
         Expenses_expendhead=`Expenses charged to other expenditure heads`,
         Extraordinary_expenses=`Prior period and extra-ordinary expenses`,
         Provns_directtax=`Provision for direct tax`)

#Expenses- Privisions/Depreciation
Expenses_Provns_Deprc<- readxl::read_excel("./Prowess/TotalExpenses/Expenses_Provisions_Depreciation.xlsx")

Expenses_Provns_Deprc <- Expenses_Provns_Deprc %>% pivot_longer(.,2:ncol(Expenses_Provns_Deprc), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Provns=`Provisions`,
         Provns_for_badloans=`Provisions for bad and doubtful advances, loans & receivables`,
         Provns_for_lossesoncontract=`Provision for  estimated losses on onerous contracts`,
         Provns_for_restorationcost=`Provision for restoration cost`,
         Provns_for_unspecifiedcntgts=`Provisions for unspecified contingencies`,
         Deprc=`Depreciation / Amortisation (net of transfer from revaluation reserves)`,
         Deprc_Amort_intangibleassets=`Depreciation & amortisation of PPE, intangible assets & investment property`,
         Deprc_PPE=`Depreciation on PPE`,
         Amort_intangibleassets=`Amortisation of intangible assets`,
         Deprc_on_investmentprpty=`Depreciation on Investment property`,
         Deprc_on_leasedoutassets=`Depreciation for the year on leased out assets (excl. Investment properties)`,
         Deprc_on_leasedinassets=`Depreciation for the year on leased in assets`)


Expenses_Provns_Deprc <- select(Expenses_Provns_Deprc,-c(Provns,Deprc))

#Expenses-Amortisation/Write-off

Expenses_Amort_Write_offs<- readxl::read_excel("./Prowess/TotalExpenses/Expenses_Amortisation_Write-off.xlsx")

Expenses_Amort_Write_offs <- Expenses_Amort_Write_offs %>% pivot_longer(.,2:ncol(Expenses_Amort_Write_offs), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Amort=`Amortisation`,
         Amort_Prelim_expenses=`Preliminary expenses amortised`,
         Amort_Capitalissue_expenses=`Capital issue expenses amortised`,
         Amort_Prdtdevp_expenses=`Product development expenses amortised`,
         Amort_Licence_fees=`Licence fees amortised`,
         Amort_Proj_Preoperative_expenses=`Project expenses and pre-operative expenses amortised`,
         Amort_Others=`Other amortisations`,
         Write_offs=`Write-offs`,
         Write_offs_Badtrade=`Bad trade and other receivables, loans & advances written off`,
         Write_offs_Assets=`Assets written off`,
         Write_offs_Biological_assets=`Biological assets other than bearer plants written off`,
         Write_offs_investment_assets=`Investment Property write-off`,
         Write_offs_Inventories=`Inventories written off / written down`,
         Write_offs_Others=`Other write-offs`)


Expenses_Amort_Write_offs <- select(Expenses_Amort_Write_offs,-c(Amort,Write_offs))     


#Expenses-Auditor-Consultancy fees

Expenses_Auditor_Consultancy_Fees<- readxl::read_excel("./Prowess/TotalExpenses/Expenses_Auditor_Consultancy_Fees.xlsx")

Expenses_Auditor_Consultancy_Fees <- Expenses_Auditor_Consultancy_Fees %>% pivot_longer(.,2:ncol(Expenses_Auditor_Consultancy_Fees), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Auditor_fees=`Auditors fees`,
         Audit_fees=`Audit fees`,
         Auditor_fees_taxation=`Auditors fees for taxation matters`,
         Auditor_fees_companylaw=`Auditors fees for company law matters & others`,
         Cnsltncy_fees=`Consultancy fees`,
         Cnsltncy_fees_auditors=`Consultancy fees to auditors`,
         Cnsltncy_fees_others=`Consultancy fees to others`)


#Expenses-ITES-Travel
Expenses_ITES_Travel<- readxl::read_excel("./Prowess/TotalExpenses/Expenses_ITES_Travel.xlsx")

Expenses_ITES_Travel <- Expenses_ITES_Travel %>% pivot_longer(.,2:ncol(Expenses_ITES_Travel), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         IT_ITES_svcs=`IT/ITES & other professional services`,
         Software_chgs=`Software charges`,
         IT_enabled_svcs_chgs=`IT enabled services charges`,
         Cost_audit_fees=`Cost audit fees`,
         Legal_chgs=`Legal charges`,
         Other_svcs=`Other professional services`,
         Adv_expenses=`Advertising expenses`,
         Mkt_expenses=`Marketing expenses`,
         Rbt_discount=`Rebates & discount expenses`,
         Sales_promo_expenses=`Sales promotion expenses`,
         Distribn_expenses=`Distribution expenses`,
         Trvl_expenses=`Travel expenses`) %>% 
  select(-Trvl_expenses)#because varr is already in Expenses11_20

#Expenses_RawMaterials_IndirectTaxes

Expenses_RawMaterials_IndirectTaxes<- readxl::read_excel("./Prowess/TotalExpenses/Expenses_RawMaterials_IndirectTaxes.xlsx")

Expenses_RawMaterials_IndirectTaxes<- Expenses_RawMaterials_IndirectTaxes %>% pivot_longer(.,2:ncol(Expenses_RawMaterials_IndirectTaxes), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Raw_materials_expenses=`Raw material expenses`,
         Stores_consumed=`Stores, spares, tools consumed`,
         Raw_material_purchased=`Raw material purchased`,
         Pkg_expenses=`Packaging and packing expenses`,
         Excise_duty=`Excise duty`,
         Sales_tax=`Sales tax`,
         Valueadded_tax=`Value added tax`,
         Goods_service_tax=`Goods and service tax`,
         Other_tax=`Other indirect taxes`,
         Indirect_tax_credits=`indirect tax credits`)

Expenses_RawMaterials_IndirectTaxes <- select(Expenses_RawMaterials_IndirectTaxes,-c(Pkg_expenses))

#Expenses_Miscellaneous
Expenses_Miscellaneous<- readxl::read_excel("./Prowess/TotalExpenses/Expenses_Miscellaneous.xlsx")

Expenses_Miscellaneous<- Expenses_Miscellaneous %>% pivot_longer(.,2:ncol(Expenses_Miscellaneous), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Misc_expend=`Miscellaneous expenditure`,
         Donations=`Donations`,
         Soc_community_expenses=`Social and community expenses`,
         Env_Pollu_ctrl_expenses=`Environment and pollution control related expenses`,
         Sub_Mem_fees=`Subscriptions and membership fees`,
         Rnd_expense=`Research & development expenses`,
         Penalities_directtaxes=`Penalties on direct taxes`,
         Other_Misc=`Other miscellaneous expenses`,
         Indirect_tax_penalities=`Indirect tax penalties`) %>% 
  select(-Misc_expend)#because the var is already in Expenses11_20


# Merging Expenses, Change in stock, Contingent Liabilities, CSR D --------

Total_Expenses<-left_join(Expenses21_32, Expenses11_20, by= c("Co_name","year")) %>%
  left_join(.,Expenses1_10, by=c("Co_name","year")) %>%
  left_join(.,Expenses_Provns_Deprc, by=c("Co_name","year")) %>%
  left_join(.,Expenses_Amort_Write_offs, by=c("Co_name","year")) %>%
  left_join(.,Expenses_Auditor_Consultancy_Fees, by=c("Co_name","year")) %>%
  left_join(.,Expenses_ITES_Travel, by=c("Co_name","year")) %>%
  left_join(.,Expenses_RawMaterials_IndirectTaxes, by=c("Co_name","year")) %>%
  left_join(.,Expenses_Miscellaneous, by=c("Co_name","year")) %>%
  left_join(.,ChangeinStock, by=c("Co_name","year")) %>%
  left_join(.,CntgtLiabilities, by=c("Co_name","year")) %>%
  left_join(.,CSR_Disclosures, by=c("Co_name","year"))

remove(Expenses1_10,Expenses11_20,Expenses21_32,Expenses_Provns_Deprc,Expenses_Amort_Write_offs,
       Expenses_Auditor_Consultancy_Fees,Expenses_ITES_Travel,Expenses_RawMaterials_IndirectTaxes,Expenses_Miscellaneous,
       ChangeinStock,CntgtLiabilities,CSR_Disclosures)

DerivedIndicatorsOfExpenses<- readxl::read_excel("./Prowess/DerivedIndicatorsOfExpenses/DerivedIndicatorsOfExpenses.xlsx")

DerivedIndicatorsOfExpenses<- DerivedIndicatorsOfExpenses %>% pivot_longer(.,2:ncol(DerivedIndicatorsOfExpenses), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Cost_goods_sold=`Cost of goods sold`,
         Cost_of_sales=`Cost of sales`,
         Cost_of_sales_per_day=`Cost of sales per day`,
         Tot_Taxes=`Total taxes`,
         Direct_taxes=`Direct taxes (incl MAT & def tax paid)`,
         Tot_Taxes_By_Tot_Income=`Total taxes / total income`,
         Tot_indirect_Taxes_By_Tot_Income=`Total indirect taxes / total income`,
         Tot_Excise_By_Indst_Sales=`Excise / industrial sales`,
         Tot_direct_taxes_By_Tot_Income=`Total direct taxes / total income`,
         Corporate_tax_By_PBT=`Corporate tax / PBT`,
         FBT_By_Compensation=`FBT / compensation to employees & travel exp`,
         Priorperiod_direct_taxes_By_Tot_Income=`Prior period direct taxes / total income`)

Total_Expenses<-left_join(DerivedIndicatorsOfExpenses, Total_Expenses, by= c("Co_name","year"))

remove(DerivedIndicatorsOfExpenses)



# Merging FV_1 and Total_Expenses ---------------------------------------------------

load("~/MCA/FV_1.Rdata")
FV_2 <- left_join(Total_Expenses, FV_1, by=c("Co_name","year"))

remove(FV_1,Total_Expenses)

#save(All_excl_Key_Pers_Rel, Associate, ENT_KMP, Indiv_SI_over_company, Joint_Venture, Others, Promoters, Shareholders, file = "./Prowess/RPT/RPTs.Rdata" )



# Assets Cleaning ---------------------------------------------------------
assets_1<- readxl::read_excel("./Prowess/assets1.xlsx")

assets<-assets_1 %>% select(1, starts_with("Tot_assets"), starts_with("Non_Curr_assets"), starts_with("Gppe"), starts_with("NFA"),
                            starts_with("GIA"), starts_with("NIA"),  starts_with("GFA"), starts_with("Curr_assets"), starts_with("Cash_bal"),
                            starts_with("Bank_bal"), starts_with("Avg_TA"),starts_with("Avg_Deb"), starts_with("Avg_nw"),
                            starts_with("Avg_cred"), starts_with("Avg_fg"))

Tot_assets<-assets_1 %>% select(1, starts_with("Tot_assets")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Tot_assets"), names_to = "year", values_to = "Tot_assets") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


Non_Curr_assets<-assets_1 %>% select(1, starts_with("Non_Curr_assets")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Non_Curr_assets"), names_to = "year", values_to = "Non_Curr_assets") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


Gppe<-assets_1 %>% select(1, starts_with("Gppe")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Gppe"), names_to = "year", values_to = "Gppe") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

NFA<-assets_1 %>% select(1, starts_with("NFA")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("NFA"), names_to = "year", values_to = "NFA") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


GIA<-assets_1 %>% select(1, starts_with("GIA")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("GIA"), names_to = "year", values_to = "GIA") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


NIA<-assets_1 %>% select(1, starts_with("NIA")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("NIA"), names_to = "year", values_to = "NIA") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


GFA<-assets_1 %>% select(1, starts_with("GFA")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("GFA"), names_to = "year", values_to = "GFA") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

Curr_assets<- assets_1 %>% select(1, starts_with("Curr_assets")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Curr_assets"), names_to = "year", values_to = "Curr_assets") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

Cash_bal<-assets_1 %>% select(1, starts_with("Cash_bal")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Cash_bal"), names_to = "year", values_to = "Cash_bal") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


Bank_bal<-assets_1 %>% select(1, starts_with("Bank_bal")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Bank_bal"), names_to = "year", values_to = "Bank_bal") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

Avg_TA<-assets_1 %>% select(1, starts_with("Avg_TA")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Avg_TA"), names_to = "year", values_to = "Avg_TA") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

Avg_Deb<-assets_1 %>% select(1, starts_with("Avg_Deb")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Avg_Deb"), names_to = "year", values_to = "Avg_Deb") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

Avg_nw<-assets_1 %>% select(1, starts_with("Avg_nw")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Avg_nw"), names_to = "year", values_to = "Avg_nw") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

Avg_cred<-assets_1 %>% select(1, starts_with("Avg_cred")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Avg_cred"), names_to = "year", values_to = "Avg_cred") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

Avg_fg<-assets_1 %>% select(1, starts_with("Avg_fg")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Avg_fg"), names_to = "year", values_to = "Avg_fg") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

assets_1<-left_join(Tot_assets, Non_Curr_assets, by= c("Co_name","year")) %>% 
  left_join(., Gppe, by= c("Co_name","year")) %>% 
  left_join(., NFA, by= c("Co_name","year")) %>% 
  left_join(., GIA, by= c("Co_name","year")) %>% 
  left_join(., NIA, by= c("Co_name","year")) %>% 
  left_join(., GFA, by= c("Co_name","year")) %>% 
  left_join(., Curr_assets, by= c("Co_name","year")) %>% 
  left_join(., Cash_bal, by= c("Co_name","year")) %>% 
  left_join(., Bank_bal, by= c("Co_name","year")) %>% 
  left_join(., Avg_TA, by= c("Co_name","year")) %>%
  left_join(., Avg_Deb, by= c("Co_name","year")) %>% 
  left_join(., Avg_nw, by= c("Co_name","year")) %>% 
  left_join(., Avg_cred, by= c("Co_name","year")) %>% 
  left_join(., Avg_fg, by= c("Co_name","year"))

remove(Tot_assets, Non_Curr_assets, Gppe, NFA, GIA, NIA, GFA, Curr_assets, Cash_bal,Bank_bal,
       Avg_TA,Avg_Deb,Avg_nw,Avg_cred, Avg_fg)



# capital Cleaning ----------------------------------------------------

capital<- readxl::read_excel("./Prowess/capital.xlsx")

capital<-capital %>% select(1, starts_with("total_capital"), starts_with("paid_eqcapital"), starts_with("fulpaid_eqcapital"), 
                            starts_with("parpaid_eqcapital"),starts_with("for_eqcap"), starts_with("paid_prefcapital"),
                            starts_with("fulpaid_prefcapital"),starts_with("parpaid_prefcapital"), starts_with("cap_contrib_by_govt_oth"),
                            starts_with("hybridorcapitalsecs"), starts_with("eqcont_govt"),
                            starts_with("moneyrec_ag_conv_sh_war"))

total_capital<-capital %>% select(1, starts_with("total_capital")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("total_capital"), names_to = "year", values_to = "total_capital") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


paid_eqcapital<-capital %>% select(1, starts_with("paid_eqcapital")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("paid_eqcapital"), names_to = "year", values_to = "paid_eqcapital") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)



fulpaid_eqcapital<-capital %>% select(1, starts_with("fulpaid_eqcapital")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("fulpaid_eqcapital"), names_to = "year", values_to = "fulpaid_eqcapital") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

parpaid_eqcapital<-capital %>% select(1, starts_with("parpaid_eqcapital")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("parpaid_eqcapital"), names_to = "year", values_to = "parpaid_eqcapital") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


for_eqcap<-capital %>% select(1, starts_with("for_eqcap")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("for_eqcap"), names_to = "year", values_to = "for_eqcap") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


paid_prefcapital<-capital %>% select(1, starts_with("paid_prefcapital")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("paid_prefcapital"), names_to = "year", values_to = "paid_prefcapital") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


fulpaid_prefcapital<-capital %>% select(1, starts_with("fulpaid_prefcapital")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("fulpaid_prefcapital"), names_to = "year", values_to = "fulpaid_prefcapital") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

parpaid_prefcapital<- capital %>% select(1, starts_with("parpaid_prefcapital")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("parpaid_prefcapital"), names_to = "year", values_to = "parpaid_prefcapital") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

cap_contrib_by_govt_oth<-capital %>% select(1, starts_with("cap_contrib_by_govt_oth")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("cap_contrib_by_govt_oth"), names_to = "year", values_to = "cap_contrib_by_govt_oth") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


hybridorcapitalsecs<-capital %>% select(1, starts_with("hybridorcapitalsecs")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("hybridorcapitalsecs"), names_to = "year", values_to = "hybridorcapitalsecs") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

eqcont_govt<-capital %>% select(1, starts_with("eqcont_govt")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("eqcont_govt"), names_to = "year", values_to = "eqcont_govt") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

moneyrec_ag_conv_sh_war<-capital %>% select(1, starts_with("moneyrec_ag_conv_sh_war")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("moneyrec_ag_conv_sh_war"), names_to = "year", values_to = "moneyrec_ag_conv_sh_war") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


capital<-left_join(total_capital, paid_eqcapital, by= c("Co_name","year")) %>% 
  left_join(., fulpaid_eqcapital, by= c("Co_name","year")) %>% 
  left_join(., parpaid_eqcapital, by= c("Co_name","year")) %>% 
  left_join(., for_eqcap, by= c("Co_name","year")) %>% 
  left_join(., paid_prefcapital, by= c("Co_name","year")) %>% 
  left_join(., fulpaid_prefcapital, by= c("Co_name","year")) %>% 
  left_join(., parpaid_prefcapital, by= c("Co_name","year")) %>% 
  left_join(., cap_contrib_by_govt_oth, by= c("Co_name","year")) %>% 
  left_join(., hybridorcapitalsecs, by= c("Co_name","year")) %>% 
  left_join(., eqcont_govt, by= c("Co_name","year")) %>%
  left_join(., moneyrec_ag_conv_sh_war, by= c("Co_name","year"))

remove(total_capital, paid_eqcapital, fulpaid_eqcapital, parpaid_eqcapital, for_eqcap, paid_prefcapital, fulpaid_prefcapital, parpaid_prefcapital, cap_contrib_by_govt_oth,hybridorcapitalsecs,
       eqcont_govt,moneyrec_ag_conv_sh_war)



# working capital ---------------------------------------------------------

workingcapital<- readxl::read_excel("./Prowess/workingcapital.xlsx")

workingcapital<-workingcapital %>% select(1, starts_with("gwc_cs"), starts_with("nwc"), starts_with("quickratio"), 
                                          starts_with("currentratio"),starts_with("debtequity"), starts_with("dscr"),
                                          starts_with("cashtocl"),starts_with("rmturnover"), starts_with("wipturnover"),
                                          starts_with("fgturnover"), starts_with("creditorsturnover"),
                                          starts_with("debtorsturnover"),starts_with("gfa_utl_ratio"), starts_with("emp_utl_ratio"), starts_with("nfa_utl_ratio"))

gwc_cs<-workingcapital %>% select(1, starts_with("gwc_cs")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("gwc_cs"), names_to = "year", values_to = "gwc_cs") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`) %>% 
  mutate(year = case_when(
    is.na(year) ~ 2000,
    TRUE ~ year))

nwc<-workingcapital %>% select(1, starts_with("nwc")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("nwc"), names_to = "year", values_to = "nwc") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`) 



quickratio<-workingcapital %>% select(1, starts_with("quickratio")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("quickratio"), names_to = "year", values_to = "quickratio") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

currentratio<-workingcapital %>% select(1, starts_with("currentratio")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("currentratio"), names_to = "year", values_to = "currentratio") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


debtequity<-workingcapital %>% select(1, starts_with("debtequity")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("debtequity"), names_to = "year", values_to = "debtequity") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


dscr<-workingcapital %>% select(1, starts_with("dscr")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("dscr"), names_to = "year", values_to = "dscr") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

cashtocl<-workingcapital %>% select(1, starts_with("cashtocl")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("cashtocl"), names_to = "year", values_to = "cashtocl") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

rmturnover<-workingcapital %>% select(1, starts_with("rmturnover")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("rmturnover"), names_to = "year", values_to = "rmturnover") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

wipturnover<-workingcapital %>% select(1, starts_with("wipturnover")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("wipturnover"), names_to = "year", values_to = "wipturnover") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

fgturnover<-workingcapital %>% select(1, starts_with("fgturnover")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("fgturnover"), names_to = "year", values_to = "fgturnover") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

creditorsturnover<-workingcapital %>% select(1, starts_with("creditorsturnover")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("creditorsturnover"), names_to = "year", values_to = "creditorsturnover") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

debtorsturnover<-workingcapital %>% select(1, starts_with("debtorsturnover")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("debtorsturnover"), names_to = "year", values_to = "debtorsturnover") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

gfa_utl_ratio<-workingcapital %>% select(1, starts_with("gfa_utl_ratio")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("gfa_utl_ratio"), names_to = "year", values_to = "gfa_utl_ratio") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

emp_utl_ratio<-workingcapital %>% select(1, starts_with("emp_utl_ratio")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("emp_utl_ratio"), names_to = "year", values_to = "emp_utl_ratio") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

nfa_utl_ratio<-workingcapital %>% select(1, starts_with("nfa_utl_ratio")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("nfa_utl_ratio"), names_to = "year", values_to = "nfa_utl_ratio") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


workingcapital<-left_join(gwc_cs, nwc, by= c("Co_name","year")) %>% 
  left_join(., quickratio, by= c("Co_name","year")) %>% 
  left_join(., currentratio, by= c("Co_name","year")) %>% 
  left_join(., debtequity, by= c("Co_name","year")) %>% 
  left_join(., dscr, by= c("Co_name","year")) %>% 
  left_join(., cashtocl, by= c("Co_name","year")) %>% 
  left_join(., rmturnover, by= c("Co_name","year")) %>% 
  left_join(., wipturnover, by= c("Co_name","year")) %>% 
  left_join(., fgturnover, by= c("Co_name","year")) %>% 
  left_join(., creditorsturnover, by= c("Co_name","year")) %>%
  left_join(., debtorsturnover, by= c("Co_name","year")) %>%
  left_join(., gfa_utl_ratio, by= c("Co_name","year")) %>% 
  left_join(., emp_utl_ratio, by= c("Co_name","year")) %>%
  left_join(., nfa_utl_ratio, by= c("Co_name","year"))

remove(gwc_cs, nwc, quickratio, currentratio, debtequity, dscr, cashtocl, rmturnover, wipturnover,fgturnover,
       creditorsturnover,debtorsturnover,gfa_utl_ratio,emp_utl_ratio,nfa_utl_ratio)





# employees and branches -------------------------------------------------

empbranches<- readxl::read_excel("./Prowess/employees_branches.xlsx")


empbranches<-empbranches %>% select(1, starts_with("Employees"), starts_with("Sh_outside_India"), starts_with("Branches"))

Employees<-empbranches %>% select(1, starts_with("Employees")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Employees"), names_to = "year", values_to = "Employees") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


Sh_outside_India<-empbranches %>% select(1,starts_with("Sh_Outside_India")) %>%
  tidyr::pivot_longer(.,cols =starts_with("Sh_Outside_India"),names_to ="year", values_to ="Sh_Outside_India") %>%
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

Branches<-empbranches %>% select(1,starts_with("Branches")) %>%
  tidyr::pivot_longer(.,cols =starts_with("Branches"),names_to ="year", values_to ="Branches") %>%
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)


empbranches<-left_join(Employees, Sh_outside_India, by= c("Co_name","year")) %>% 
  left_join(., Branches, by= c("Co_name","year"))

remove(Employees, Sh_outside_India, Branches)






# RM Consumed -------------------------------------------------------------
rmconsumed<- readxl::read_excel("./Prowess/rmconsumed.xlsx")

rmconsumed<-rmconsumed %>% select(1, starts_with("rmc"), starts_with("irmc"), starts_with("importedrmc"), 
                                  starts_with("otherrmc"),starts_with("sto_sparesconsumed"), starts_with("Exp_earnings"),
                                  starts_with("exporttosales_ratio"),starts_with("tforexearnings_ti_ratio"), 
                                  starts_with("rmimportstormpurchases_ratio"))

rmc<-rmconsumed %>% select(1, starts_with("rmc")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("rmc"), names_to = "year", values_to = "rmc") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

irmc<-rmconsumed %>% select(1, starts_with("irmc")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("irmc"), names_to = "year", values_to = "irmc") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

importedrmc<-rmconsumed %>% select(1, starts_with("importedrmc")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("importedrmc"), names_to = "year", values_to = "importedrmc") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

otherrmc<-rmconsumed %>% select(1, starts_with("otherrmc")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("otherrmc"), names_to = "year", values_to = "otherrmc") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

sto_sparesconsumed<-rmconsumed %>% select(1, starts_with("sto_sparesconsumed")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("sto_sparesconsumed"), names_to = "year", values_to = "sto_sparesconsumed") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

Exp_earnings<-rmconsumed %>% select(1, starts_with("Exp_earnings")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("Exp_earnings"), names_to = "year", values_to = "Exp_earnings") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

exporttosales_ratio<-rmconsumed %>% select(1, starts_with("exporttosales_ratio")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("exporttosales_ratio"), names_to = "year", values_to = "exporttosales_ratio") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

tforexearnings_ti_ratio<-rmconsumed %>% select(1, starts_with("tforexearnings_ti_ratio")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("tforexearnings_ti_ratio"), names_to = "year", values_to = "tforexearnings_ti_ratio") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

rmimportstormpurchases_ratio<-rmconsumed %>% select(1, starts_with("rmimportstormpurchases_ratio")) %>% 
  tidyr::pivot_longer(.,cols = starts_with("rmimportstormpurchases_ratio"), names_to = "year", values_to = "rmimportstormpurchases_ratio") %>% 
  mutate(year=as.double(str_sub(year, start=-4L))) %>% rename(Co_name = `Company Name`)

rmconsumed<-left_join(rmc, irmc, by= c("Co_name","year")) %>% 
  left_join(., importedrmc, by= c("Co_name","year")) %>% 
  left_join(., otherrmc, by= c("Co_name","year")) %>% 
  left_join(., sto_sparesconsumed, by= c("Co_name","year")) %>% 
  left_join(., Exp_earnings, by= c("Co_name","year")) %>% 
  left_join(., exporttosales_ratio, by= c("Co_name","year")) %>% 
  left_join(., tforexearnings_ti_ratio, by= c("Co_name","year")) %>% 
  left_join(., rmimportstormpurchases_ratio, by= c("Co_name","year"))

remove(rmc, irmc, importedrmc, otherrmc, sto_sparesconsumed, Exp_earnings, exporttosales_ratio, tforexearnings_ti_ratio,rmimportstormpurchases_ratio)

# liabilities -------------------------------------------------------------
Liab_1<- readxl::read_excel("./Prowess/Liabilities/Liab_1.xlsx")

Liab_1<- Liab_1 %>% pivot_longer(.,2:(ncol(Liab_1)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)

Liab_2<- readxl::read_excel("./Prowess/Liabilities/Liab_2.xlsx")

Liab_2<- Liab_2 %>% pivot_longer(.,2:(ncol(Liab_2)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)


Liab_3<- readxl::read_excel("./Prowess/Liabilities/Liab_3.xlsx")

Liab_3<-  Liab_3 %>% pivot_longer(.,2:(ncol(Liab_3)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)


Liab_4<- readxl::read_excel("./Prowess/Liabilities/Liab_4.xlsx")

Liab_4<- Liab_4 %>% pivot_longer(.,2:(ncol(Liab_4)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)


Liab_5<- readxl::read_excel("./Prowess/Liabilities/Liab_5.xlsx")

Liab_5<- Liab_5 %>% pivot_longer(.,2:(ncol(Liab_5)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)


Deposits<- readxl::read_excel("./Prowess/Liabilities/Deposits.xlsx")

Deposits<- Deposits %>% pivot_longer(.,2:(ncol(Deposits)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)


Liabilities_deposits<-left_join(Liab_1, Liab_2, by= c("Co_name","year")) %>% 
  left_join(., Liab_3, by= c("Co_name","year")) %>% 
  left_join(., Liab_4, by= c("Co_name","year")) %>% 
  left_join(., Liab_5, by= c("Co_name","year")) %>% 
  left_join(., Deposits, by= c("Co_name","year"))

remove(Liab_1, Liab_2, Liab_3, Liab_4, Liab_5, Deposits)

# Directors Salary --------------------------------------------------------
Dir_salary<- readxl::read_excel("./Prowess/directors_kmp_remuneration.xlsx")

Dir_salary<- Dir_salary %>% pivot_longer(.,2:(ncol(Dir_salary)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% 
  rename(Co_name = `Company Name`,
         Dir_bonus_and_commission =`Dir_bonus and commission`  )


# cfo ---------------------------------------------------------------------

cfo<- readxl::read_excel("./Prowess/CFO.xlsx")

cfo<- cfo %>% pivot_longer(.,2:(ncol(cfo)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)



# cfo_forex ---------------------------------------------------------------


cfo_forex<- readxl::read_excel("./Prowess/CFO_FOREX_fmt.xlsx")


cfo_forex<- cfo_forex %>% pivot_longer(.,2:(ncol(cfo_forex)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Net_CashFlow_OperAct =`Net cash flow from operating activities`,
         Net_CashInOutFlow_InvAct =`Net cash inflow or (outflow) from investing activities`,
         Net_CashInOutFlow_FinAct =`Net cash inflow or (outflow) from financing activities`,
         Cash_in_Yrbegin =`Cash and cash equivalents as at the beginning of the year`,
         Cash_in_Yrend =`Cash and cash equivalents as at the end of the year`,
         Tot_forex_earngs =`Total forex earnings`,
         Export_goods =`Export of goods(fob)`,
         Forex_earngs_interest =`Forex earning -- interest`,
         Forex_earngs_dvnd =`Forex earning -- dividend`,
         Other_forex_earngs =`Other forex earnings`,
         Deemed_rpt =`Deemed export`,
         Export_svcs =`Export of services`)





# ASK sai to rename vars of liab's, Dir Salary, cf0, cfo_forex 


# Auditor Name ------------------------------------------------------------

Auditors<-readxl::read_excel("./Prowess/Auditors/auditornames.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`)) %>% 
  rename(Co_name = `Company Name`,
         Auditor_name = `Auditor Name`)
# promoter Holding --------------------------------------------------------


promhol<-readxl::read_excel("./Prowess/Promoters_shareholding.xlsx")

promhol<- promhol %>% pivot_longer(.,2:(ncol(promhol)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)


# Return Ratios -----------------------------------------------------------

return_ratios<-readxl::read_excel("./Prowess/return_ta_nw_capemp.xlsx")

return_ratios<- return_ratios %>% pivot_longer(.,2:(ncol(return_ratios)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)

#NSE Indicators
nse_indicators<- readxl::read_excel("./Prowess/NSElist_identity_indicators.xlsx")


# Receivables and Inventory -------------------------------------------------------------
Receivables_inventory<-readxl::read_excel("./Prowess/Receivables_Inventories.xlsx")

Receivables_inventory<- Receivables_inventory %>% pivot_longer(.,2:(ncol(Receivables_inventory)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)

#eeptools::isid(Receivables_inventory, vars = c("Co_name", "year"))

# NSE Indicators ----------------------------------------------------------
NSE_indicators<-readxl::read_excel("./Prowess/NSElist_identity_indicators.xlsx")

# Creating FV final --------------------------------------------------------


FV<-left_join(FV_2, assets_1, by=c("Co_name","year")) %>%
  left_join(., Liabilities_deposits, by= c("Co_name","year")) %>%
  left_join(., capital, by= c("Co_name","year")) %>% 
  left_join(., workingcapital, by= c("Co_name","year")) %>% 
  left_join(., rmconsumed, by= c("Co_name","year")) %>%
  left_join(., empbranches, by= c("Co_name","year")) %>%
  left_join(., Dir_salary, by= c("Co_name","year")) %>% 
  left_join(., cfo, by= c("Co_name","year")) %>% 
  left_join(., cfo_forex, by= c("Co_name","year")) %>%
  left_join(., promhol, by= c("Co_name","year")) %>% 
  left_join(., return_ratios, by= c("Co_name","year")) %>% 
  left_join(., Receivables_inventory, by= c("Co_name","year")) %>%
  left_join(., NSE_indicators, by= c("Co_name")) 


Aud_NSE<-left_join(Auditors, NSE_indicators, by= c("Co_name"))                         

Aud_NSE<-Aud_NSE %>% filter(Indust_type!=3) #dups verified and then filtered to remove banks. wait until confirmation to merge with FV

remove(FV_2, assets_1, Liabilities_deposits, capital, workingcapital, rmconsumed, empbranches, Dir_salary, cfo, cfo_forex,
       promhol, return_ratios, Receivables_inventory)





# All Party Types ---------------------------------------------------------
APT_1<-readxl::read_excel("./Prowess/RPT/All Party Types/All Party Types 1.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
APT_2<-readxl::read_excel("./Prowess/RPT/All Party Types/All Party Types 2.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
APT_3<-readxl::read_excel("./Prowess/RPT/All Party Types/All Party Types 3.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
APT_4<-readxl::read_excel("./Prowess/RPT/All Party Types/All Party Types 4.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
APT_5<-readxl::read_excel("./Prowess/RPT/All Party Types/All Party Types 5.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
APT_6<-readxl::read_excel("./Prowess/RPT/All Party Types/All Party Types 6.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
APT_7<-readxl::read_excel("./Prowess/RPT/All Party Types/All Party Types 7.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
APT_8<-readxl::read_excel("./Prowess/RPT/All Party Types/All Party Types 8.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))

APT<-bind_rows(APT_1, APT_2, APT_3, APT_4, APT_5, APT_6, APT_7, APT_8) %>% 
  select(-2, -3, -6, -9) %>% 
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         LoC_given_for_RP = `LoC/Stand by LoC given on behalf of related parties (conting.liab.)`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Share_application_money_given_OS_asset= `Share Application Money given o/s (Asset)`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         SA_money_received_OS_liab = `Share Application Money received o/s (liab.)`,
         SA_money_received_during_year = `Share Application Money received during the year`,
         Margin_money_paid_during_year = `Margin Money Paid during the year`,
         SA_money_given_during_year = `Share Application Money given during the year`,
         Margin_money_paid_OS_asset = `Margin Money Paid o/s (Asset)`,
         Transaction_not_specified= `Transaction not specified`,
         OS_close_bal_of_FA = `Outstanding/Closing balance of fixed assets`,
         Margin_money_received_during_year= `Margin Money Received during the year`,
         Margin_money_received_OS_liab = `Margin Money Recd. o/s (liab.)`)

remove(APT_1, APT_2, APT_3, APT_4, APT_5, APT_6, APT_7, APT_8)

#eeptools::isid(APT, vars = c("Co_name","year", "Party_name"))


# Fellow Subsidiary Company-----------------------------------------------

FSC_1<-readxl::read_excel("./Prowess/RPT/Fellow Subsidiary company/1.xlsx") 

FSC_2<-readxl::read_excel("./Prowess/RPT/Fellow Subsidiary company/2.xlsx") 
FSC_3<-readxl::read_excel("./Prowess/RPT/Fellow Subsidiary company/3.xlsx") 

FSC_1<-FSC_1 %>% mutate(year = lubridate::year(`Slot Date`)) %>% filter(year!=2010)
FSC_2<-FSC_2 %>% mutate(year = lubridate::year(`Slot Date`)) %>% filter(year!=2015)
FSC_3<-FSC_3 %>% mutate(year = lubridate::year(`Slot Date`))

FSC<-bind_rows(FSC_1, FSC_2, FSC_3) %>% 
  select(-2, -3, -6, -9) %>% 
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         LoC_given_for_RP = `LoC/Stand by LoC given on behalf of related parties (conting.liab.)`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`)

remove(FSC_1, FSC_2, FSC_3)

#eeptools::isid(FSC, vars = c("Co_name","year", "Party_name"))





# Associate --------------------------------------------------------------

Associate<-readxl::read_excel("./Prowess/RPT/Associate/Associate.xlsx")

Associate<- Associate %>% mutate(year = lubridate::year(`Slot Date`)) %>% 
  select(-2, -3, -6, -9) %>%
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         LoC_given_for_RP = `LoC/Stand by LoC given on behalf of related parties (conting.liab.)`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Share_application_money_given_OS_asset= `Share Application Money given o/s (Asset)`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         SA_money_received_OS_liab = `Share Application Money received o/s (liab.)`,
         SA_money_received_during_year = `Share Application Money received during the year`,
         Margin_money_paid_during_year = `Margin Money Paid during the year`,
         SA_money_given_during_year = `Share Application Money given during the year`,
         Transaction_not_specified= `Transaction not specified`,
         OS_close_bal_of_FA = `Outstanding/Closing balance of fixed assets`,
         Margin_money_received_during_year= `Margin Money Received during the year`,
         Margin_money_received_OS_liab = `Margin Money Recd. o/s (liab.)`)

#eeptools::isid(Associate, vars = c("Co_name","year", "Party_name"))




# Joint Venture -----------------------------------------------------------
Joint_Venture<-readxl::read_excel("./Prowess/RPT/Joint Venture/Joint Venture.xlsx")
Joint_Venture<- Joint_Venture %>% mutate(year = lubridate::year(`Slot Date`)) %>%
  select(-2, -3, -6, -9) %>% 
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>%
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         LoC_given_for_RP = `LoC/Stand by LoC given on behalf of related parties (conting.liab.)`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         SA_money_given_during_year = `Share Application Money given during the year`,
         Share_application_money_given_OS_asset= `Share Application Money given o/s (Asset)`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         SA_money_received_during_year = `Share Application Money received during the year`,
         Transaction_not_specified= `Transaction not specified`,
         OS_close_bal_of_FA = `Outstanding/Closing balance of fixed assets`,
         Margin_money_received_OS_liab = `Margin Money Recd. o/s (liab.)`)

#eeptools::isid(Joint_Venture, vars = c("Co_name","year", "Party_name"))


# Promoters ---------------------------------------------------------------

Promoters<-readxl::read_excel("./Prowess/RPT/Promoters/Promoters.xlsx")

Promoters<- Promoters %>% mutate(year = lubridate::year(`Slot Date`)) %>%
  select(-2, -3, -6, -9) %>%
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         SA_money_received_during_year = `Share Application Money received during the year`,)
#eeptools::isid(Promoters, vars = c("Co_name","year", "Part_name"))



# Ent over which KMP have control -----------------------------------------


ENT_KMP_1<-readxl::read_excel("./Prowess/RPT/ENT over KMP/1.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))
ENT_KMP_2<-readxl::read_excel("./Prowess/RPT/ENT over KMP/2.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))
ENT_KMP_3<-readxl::read_excel("./Prowess/RPT/ENT over KMP/3.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))
ENT_KMP_4<-readxl::read_excel("./Prowess/RPT/ENT over KMP/4.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))

ENT_KMP<-bind_rows(ENT_KMP_1, ENT_KMP_2, ENT_KMP_3, ENT_KMP_4) %>%
  select(-2, -3, -6, -9) %>%
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Share_application_money_given_OS_asset= `Share Application Money given o/s (Asset)`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         SA_money_received_OS_liab = `Share Application Money received o/s (liab.)`,
         SA_money_received_during_year = `Share Application Money received during the year`,
         SA_money_given_during_year = `Share Application Money given during the year`,
         Margin_money_paid_OS_asset = `Margin Money Paid o/s (Asset)`,
         OS_close_bal_of_FA = `Outstanding/Closing balance of fixed assets`,
         Margin_money_received_during_year= `Margin Money Received during the year`,
         Margin_money_received_OS_liab = `Margin Money Recd. o/s (liab.)`)

remove(ENT_KMP_1, ENT_KMP_2, ENT_KMP_3, ENT_KMP_4)

#eeptools::isid(Associate, vars = c("Co_name","year", "Party_name"))





# Indiv having SI over company --------------------------------------------

Indiv_SI_over_company<-readxl::read_excel("./Prowess/RPT/Indiv having SI over company/Indiv having SI over company.xlsx")

Indiv_SI_over_company<- Indiv_SI_over_company %>% mutate(year = lubridate::year(`Slot Date`)) %>%
  select(-2, -3, -6, -9) %>%
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         SA_money_received_OS_liab = `Share Application Money received o/s (liab.)`,
         Transaction_not_specified= `Transaction not specified`,
         Margin_money_received_during_year= `Margin Money Received during the year`,
         Margin_money_received_OS_liab = `Margin Money Recd. o/s (liab.)`)

#eeptools::isid(Indiv_SI_over_company, vars = c("Co_name","year", "Party_name"))





# Shareholders ------------------------------------------------------------
Shareholders<-readxl::read_excel("./Prowess/RPT/Shareholders/Shareholders.xlsx")

Shareholders<- Shareholders %>% mutate(year = lubridate::year(`Slot Date`)) %>%
  select(-2, -3, -6, -9) %>%
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>%
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,)

#eeptools::isid(Shareholders, vars = c("Co_name","year", "Party_name"))






# Others ------------------------------------------------------------------

Others_1<-readxl::read_excel("./Prowess/RPT/Others/1.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))
Others_2<-readxl::read_excel("./Prowess/RPT/Others/2.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))
Others_3<-readxl::read_excel("./Prowess/RPT/Others/3.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))
Others_4<-readxl::read_excel("./Prowess/RPT/Others/4.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))

Others<-bind_rows(Others_1, Others_2, Others_3, Others_4) %>%
  
  select(-2, -3, -6, -9) %>%
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         LoC_given_for_RP = `LoC/Stand by LoC given on behalf of related parties (conting.liab.)`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         SA_money_given_during_year = `Share Application Money given during the year`,
         Transaction_not_specified= `Transaction not specified`,)

remove(Others_1, Others_2, Others_3, Others_4)

#eeptools::isid(Others, vars = c("Co_name","year", "Party_name"))

# All excl Key Pers & Rel -------------------------------------------------
AEKP_1<-readxl::read_excel("./Prowess/RPT/All excl Key Pers & Rel/1.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))
AEKP_2<-readxl::read_excel("./Prowess/RPT/All excl Key Pers & Rel/2.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
AEKP_3<-readxl::read_excel("./Prowess/RPT/All excl Key Pers & Rel/3.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
AEKP_4<-readxl::read_excel("./Prowess/RPT/All excl Key Pers & Rel/4.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
AEKP_5<-readxl::read_excel("./Prowess/RPT/All excl Key Pers & Rel/5.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
AEKP_6<-readxl::read_excel("./Prowess/RPT/All excl Key Pers & Rel/6.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
AEKP_7<-readxl::read_excel("./Prowess/RPT/All excl Key Pers & Rel/7.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))

All_excl_Key_Pers_Rel<-bind_rows(AEKP_1, AEKP_2, AEKP_3, AEKP_4, AEKP_5, AEKP_6, AEKP_7) %>% 
  select(-2, -3, -6, -9) %>% 
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         LoC_given_for_RP = `LoC/Stand by LoC given on behalf of related parties (conting.liab.)`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Share_application_money_given_OS_asset= `Share Application Money given o/s (Asset)`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         SA_money_received_OS_liab = `Share Application Money received o/s (liab.)`,
         SA_money_received_during_year = `Share Application Money received during the year`,
         Margin_money_paid_during_year = `Margin Money Paid during the year`,
         SA_money_given_during_year = `Share Application Money given during the year`,
         Margin_money_paid_OS_asset = `Margin Money Paid o/s (Asset)`,
         Transaction_not_specified= `Transaction not specified`,
         OS_close_bal_of_FA = `Outstanding/Closing balance of fixed assets`,
         Margin_money_received_during_year= `Margin Money Received during the year`,
         Margin_money_received_OS_liab = `Margin Money Recd. o/s (liab.)`) %>% 
  filter(Party_type!="All Party Types")

remove(AEKP_1, AEKP_2, AEKP_3, AEKP_4, AEKP_5, AEKP_6, AEKP_7)

#eeptools::isid(All_excl_Key_Pers_Rel, vars = c("Co_name","year", "Party_name"))



# keypersonnel ------------------------------------------------------------

KP_1<-readxl::read_excel("./Prowess/RPT/keypersonnel/1.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`))
KP_2<-readxl::read_excel("./Prowess/RPT/keypersonnel/2.xlsx")%>% 
  mutate(year = lubridate::year(`Slot Date`))
KP_3<-readxl::read_excel("./Prowess/RPT/keypersonnel/3.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
KP_4<-readxl::read_excel("./Prowess/RPT/keypersonnel/4.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))%>% group_by(`Company Name`, year, `Party Name`, `Transaction Type`) %>% 
  mutate(year = lubridate::year(`Slot Date`)) %>% 
  distinct()
KP_5<-readxl::read_excel("./Prowess/RPT/keypersonnel/5.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
KP_6<-readxl::read_excel("./Prowess/RPT/keypersonnel/6.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
KP_7<-readxl::read_excel("./Prowess/RPT/keypersonnel/7.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
KP_8<-readxl::read_excel("./Prowess/RPT/keypersonnel/8.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
KP_9<-readxl::read_excel("./Prowess/RPT/keypersonnel/9.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))


KP<-bind_rows(KP_1, KP_2, KP_3, KP_4, KP_5, KP_6, KP_7, KP_8, KP_9) %>%
  select(-2, -3, -6, -9) %>% 
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         SA_money_received_OS_liab = `Share Application Money received o/s (liab.)`,
         SA_money_received_during_year = `Share Application Money received during the year`,
         SA_money_given_during_year = `Share Application Money given during the year`,
         Transaction_not_specified= `Transaction not specified`,
         OS_close_bal_of_FA = `Outstanding/Closing balance of fixed assets`,
         Margin_money_received_OS_liab = `Margin Money Recd. o/s (liab.)`)


remove(KP_1, KP_2, KP_3, KP_4, KP_5, KP_6, KP_7, KP_8, KP_9)





# HC and UHC --------------------------------------------------------------

HC_UHC_1<-readxl::read_excel("./Prowess/RPT/HC_UHC/RPT_HC_UHC_2000-2004.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
HC_UHC_2<-readxl::read_excel("./Prowess/RPT/HC_UHC/RPT_HC_UHC_2005-2020.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
HC_UHC<-bind_rows(HC_UHC_1,HC_UHC_2) %>% 
  select(-2, -3, -6, -9) %>% 
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Total_cap_receipts = `Total capital receipts`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         LoC_given_for_RP = `LoC/Stand by LoC given on behalf of related parties (conting.liab.)`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         Transaction_not_specified= `Transaction not specified`)

remove(HC_UHC_1,HC_UHC_2)

#eeptools::isid(HC_UHC, vars = c("Co_name","year", "Party_name"))




# Intermediate HC ---------------------------------------------------------

Intermediate_HC<-readxl::read_excel("./Prowess/RPT/IntermediateHC/RPT_IntermediateHC_2000-2020_new.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))%>% 
  select(-2, -3, -6, -9) %>% 
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Total_cap_account_payments = `Total capital account payments`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         OS_guarantees_taken = `Outstanding guarantees taken`,SA_money_received_OS_liab = `Share Application Money received o/s (liab.)`)
#eeptools::isid(Intermediate_HC, vars = c("Co_name","year", "Party_name"))


# Parties Control Exists --------------------------------------------------

P_CE_1<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2000_2003.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_2<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2004.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_3<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2005.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_4<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2006.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_5<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2007_2008.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_6<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2009_2010.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_7<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2011.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_8<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2012.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_9<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2013.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_10<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2014.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_11<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2015.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_12<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2016_17.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_13<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2018_19.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
P_CE_14<-readxl::read_excel("./Prowess/RPT/Parties_Control_Exists/RPT_Parties_ControlExists_2020.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))

P_CE<-bind_rows(P_CE_1,P_CE_2,P_CE_3,P_CE_4,P_CE_5,P_CE_6,P_CE_7,P_CE_8,P_CE_9,P_CE_10,P_CE_11,P_CE_12,P_CE_13,P_CE_14) %>% 
  select(-2, -3, -6, -9) %>% 
  pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Share_application_money_given_OS_asset= `Share Application Money given o/s (Asset)`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         SA_money_given_during_year = `Share Application Money given during the year`,
         Transaction_not_specified= `Transaction not specified`,
         OS_close_bal_of_FA = `Outstanding/Closing balance of fixed assets`,Margin_money_paid_during_year = `Margin Money Paid during the year`,)
remove(P_CE_1,P_CE_2,P_CE_3,P_CE_4,P_CE_5,P_CE_6,P_CE_7,P_CE_8,P_CE_9,P_CE_10,P_CE_11,P_CE_12,P_CE_13,P_CE_14)


#eeptools::isid(P_CE, vars = c("Co_name","year", "Party_name"))




# Subsidiary ---------------------------------------------------------

SUB_1<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2000-2003.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_2<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2004-2006.xlsx") %>% mutate(year = lubridate::year(`Slot Date`))

SUB_3<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2007-2009.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_4<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2010-2011.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_5<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2012.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_6<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2013.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_7<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2014.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_8<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2015.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_9<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2016.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_10<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2017.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_11<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2018.xlsx") %>% group_by(`Company Name`, `Party Name`, `Transaction Type`) %>% mutate(year = lubridate::year(`Slot Date`), 
                                                                                                                                                          dups = max(row_number(`Transaction Type`)))
SUB_11 <- SUB_11 %>% filter(dups !=2)


SUB_12<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2019.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))
SUB_13<-readxl::read_excel("./Prowess/RPT/Subsidiary/RPT_Subsidiary_2020.xlsx") %>% 
  mutate(year = lubridate::year(`Slot Date`))

SUB<-bind_rows(SUB_1, SUB_2, SUB_3, SUB_4, SUB_5, SUB_6, SUB_7, SUB_8, SUB_9, SUB_10, SUB_11, SUB_12, SUB_13) %>% select(-2, -3, -6, -9, -11) %>% arrange(`Company Name`, `year`, `Party Name`) %>% pivot_wider(names_from = "Transaction Type", values_from = "Transaction Value") %>% 
  rename(Co_name = `Company Name`,
         Party_type = `Party Type`,
         Party_name = `Party Name`,
         Total_revenue_receipts= `Total revenue receipts/income`,
         Interest_income_from_RP = `Interest income from related parties`,
         Total_revenue_receipts_asp_Total_Income = `Total revenue receipts/income as % of Total income`,
         Other_transactions = `Other transactions`,
         Income_from_services_to_RP = `Income from services to related parties`,
         Total_revenue_expenses_payments = `Total revenue expenses/payments`,
         Pay_for_energy_power_fuel = `Payment for energy, power and fuel`,
         Payment_for_other_revenue_expenses = `Payment for other revenue expenses`,
         Total_revenue_expenses_asp_Total_Expenses = `Total revenue expenses/payments as % of Total expenses`,
         Net_OS_current_receivables_payables = `Net outstanding current receivables/payables`,
         CA = `Current assets`,
         Reimbursement_of_expenses_by_RP = `Reimbursement of expenses by related party`,
         Pay_for_marketing_expenses = `Payment for marketing expenses`,
         Expenses_reimbursed_to_RP = `Expenses reimbursed to related party`,
         Net_OS_borrowings_taken_or_loan_given = `Net outstanding borrowings taken/loan given`,
         OS_loans_advances_given = `Outstanding loans and advances given`,
         CL = `Current liabilities`,
         Income_from_SGRP = `Income from sale of goods to related parties`,
         Pay_for_RawM_fingoods = `Payment for raw material/fin. goods`,
         Max_amt_payable_RP_during_year = `Maximum amount payable to related party during the year`,
         Max_amt_receivable_from_RP_during_year = `Maximum amount receivable from related party during the year`,
         Total_cap_account_payments = `Total capital account payments`,
         Pay_for_investments = `Payment for investments`,
         OS_guarantees_given = `Outstanding guarantees given`,
         Other_income_from_RP = `Other income from related parties`,
         Pay_for_interest = `Payment for interest`,
         Rent_income_from_RP = `Rent income from related parties`,
         OS_loans_advances_taken = `Outstanding loans and advances taken`,
         Pay_for_royalties_tech_fees = `Payment for royalties/technical know-how fees`,
         Pay_for_FA_purchase = `Payment for fixed assets purchases`,
         Total_cap_receipts = `Total capital receipts`,
         FA_sale_receipts = `Receipts from sale of fixed assets`,
         Pay_for_other_operating_expenses = `Payment for other operating expenses`,
         Pay_for_dividend = `Payment for dividend`,
         Pay_for_processing_charges =  `Payment for processing charges/jobworks`,
         Guarantees_taken_during_year = `Guarantees taken during the year`,
         Investment_sale_receipts = `Receipts from sale of investments`,
         OS_close_bal_of_investments = `Outstanding/Closing balance of investments`,
         SC_issued_during_year = `Share capital issued during the year`,
         OS_SC = `Outstanding share capital`,
         Rent_paid = `Payment for rent`,
         Dividend_income_from_RP = `Dividend income from related parties`,
         Services_not_specified_as_given_received = `Services not specified as given or received`,
         Guarantees_given_during_year = `Guarantees given during the year`,
         OS_guarantees_taken = `Outstanding guarantees taken`,
         OS_deposits_placed = `Outstanding deposits placed`,
         LoC_given_for_RP = `LoC/Stand by LoC given on behalf of related parties (conting.liab.)`,
         Dividends_not_specified_as_given_received = `Dividends not specified as given or received`,
         Doubtful_debt_provisions = `Provision for doubtful debts`,
         Salary_wage_pay_to_RP = `Payment for salaries and wages to related parties`,
         Rent_not_specified_as_given_received = `Rent not specified as given or received`,
         Share_application_money_given_OS_asset= `Share Application Money given o/s (Asset)`,
         Loans_not_specified_as_given_received =`Loans not specified as given or received`,
         Interest_not_specified_as_given_received = `Interest not specified as given or received`,
         SA_money_received_OS_liab = `Share Application Money received o/s (liab.)`,
         SA_money_received_during_year = `Share Application Money received during the year`,
         Margin_money_paid_during_year = `Margin Money Paid during the year`,
         SA_money_given_during_year = `Share Application Money given during the year`,
         Margin_money_paid_OS_asset = `Margin Money Paid o/s (Asset)`,
         Transaction_not_specified= `Transaction not specified`,
         OS_close_bal_of_FA = `Outstanding/Closing balance of fixed assets`,
         Margin_money_received_during_year= `Margin Money Received during the year`,
         Margin_money_received_OS_liab = `Margin Money Recd. o/s (liab.)`)


remove(SUB_1, SUB_2, SUB_3, SUB_4, SUB_5, SUB_6, SUB_7, SUB_8, SUB_9, SUB_10, SUB_11, SUB_12, SUB_13)

# Appending RPT's ---------------------------------------------------------

RPT<-bind_rows(All_excl_Key_Pers_Rel, APT, Associate, ENT_KMP, FSC, HC_UHC,
               Indiv_SI_over_company, Intermediate_HC, Joint_Venture, KP, Others,
               P_CE, Promoters, Shareholders, SUB)

remove(All_excl_Key_Pers_Rel, APT, Associate, ENT_KMP, FSC, HC_UHC,
       Indiv_SI_over_company, Intermediate_HC, Joint_Venture, KP, Others,
       P_CE, Promoters, Shareholders, SUB)




FV_RPT<-left_join(RPT, FV, by=c("Co_name", "year"))

# Merging FV and RPT ------------------------------------------------------
FV_RPT<-left_join(RPT, FV, by=c("Co_name", "year"))


# Creating Lags in FV -----------------------------------------------------
FV<-FV %>% group_by(Co_name) %>% 
  mutate(across(c("Roa", "Tot_assets"), ~ lag(.,n=1L), .names = "Lag_{.col}")) %>% 
  ungroup()




# Saving Image ------------------------------------------------------------
save.image("./Prowess/Prowess Data.Rdata")

