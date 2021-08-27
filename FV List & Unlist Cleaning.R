library(tidyverse)

# Profit 1 Cleaning ---------------------------------------------------------
Profit_1<- readxl::read_excel("./Prowess/Profit/Profit 1.xlsx")


Profit_1 <- Profit_1 %>% pivot_longer(.,2:(ncol(Profit_1)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Profit_after_tax = `Profit after tax`,
         PBDITA  = PBDITA,
         PBPT = PBPT,
         PBT = PBT,
         PBIT_net = `PBIT net of P&E&OI&FI`,
         PBIT = `PBIT`,
         Cash_profit = `Cash profit`,
         PAT = `PAT net of P&E`,
         Cash_profit_net_PE = `Cash profit net of P&E`,
         OP_profit_NFC = `Operating  profit of non-financial companies`,
         OP_profit_FC = `Operating profit of financial companies`)


# Profit 2 Cleaning ---------------------------------------------------------
Profit_2<- readxl::read_excel("./Prowess/Profit/Profit 2.xlsx")

Profit_2<- Profit_2 %>% pivot_longer(.,2:(ncol(Profit_2)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company name`,
         PBPT_net_fin = `PBPT net of P&E&OI to inc fin serv`,
         PBPT_net_emp = `PBPT net of P&E&OI per employee`,
         PAT_continuing = `PAT from continuing ops as % of income from continuing ops`,
         PAT_discontinuing = `PAT discont ops as % of income from disocont ops`,
         PAT_from_continuing = `Profit after tax from continuing operations`,
         P_L_on_discontinuing = `Profit/loss after tax on discontinuing operations`,
         NPBT_Extra_Ordinary = `Net profit before tax and extra ordinary items`)

Profit= left_join(Profit_1, Profit_2, by= c("Co_name","year")) 

remove(Profit_1, Profit_2)

# Income 1 Cleaning -------------------------------------------------------
Income_1<- readxl::read_excel("./Prowess/Income/Income 1.xlsx")        

Income_1 <- Income_1 %>% pivot_longer(.,2:(ncol(Income_1)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,        
         TI = `Total income`,
         Sales =  `Sales`,
         Income_Financial =  `Income from financial services`,
         Other_Income = `Other income`,
         Industrial_sales = `Industrial sales`,
         Income_non_fin = `Income from non financial services`,
         Sales_returns = `Sales returns`,
         Trade_discount = `Trade discount`,
         FBFSI = `Fee based financial services income`,
         FundBFSI = `Fund based financial services income`,
         Other_fiancial = `Other financial services income`,
         Expenses_recovered = `Expenses recovered`,
         LDCR = `Liquidated damages and claims received`,
         Amortaisation = `Amortisation of deferred income`,
         Government_Grant = `Revenue government grant`,
         Miscellaneous = `Miscellaneous income` )



# Income 2 Cleaning -------------------------------------------------------
Income_2<- readxl::read_excel("./Prowess/Income/Income 2.xlsx")

Income_2 <- Income_2 %>% pivot_longer(.,2:(ncol(Income_2)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         PP_Extra_Ordinary = `Prior period and extra ordinary income`,
         Prior_period	= `Prior period income`,
         Extra_ordinary	= `Extra ordinary income`,
         Capitalised	= `Income capitalised`,
         Interest_Capitalised	= `Interest income capitalised`,
         DRE = `Income transferred to DRE`,
         TDS	= `Tax deducted at source (TDS)`,
         Internal_Transfers	= `Internal transfers`,
         TI_Extra_Ordinary	= `Total income net of Prior period and extra ordinary income`,
         Sales_CIS = `Sales and change in stocks`,
         Net_Sales = `Net sales`,
         Sales_NFA = `Sales Net fixed assets`)




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


FV_1<-left_join(Profit, Income, by= c("Co_name", "year")) %>% 
  left_join(., Stock_prices_capital_changes, by= c("Co_name", "year"))

#eeptools::isid(FV_1, vars = c("Co_name", "year"))


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
       Avg_TA,Avg_Deb,Avg_nw,Avg_cred, Avg_fg, assets)



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
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`,
                                                                      Dec_work_capital =`Dec_work_ capital`)


Liab_5<- readxl::read_excel("./Prowess/Liabilities/Liab_5.xlsx")

Liab_5<- Liab_5 %>% pivot_longer(.,2:(ncol(Liab_5)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`,
                                                                      Net_def_taxliabilities =`Net _def_taxliabilities`,
                                                                      Net_def_taxliabilitiesasofnetworth =`Net _def_taxliabilitiesasofnetworth`,
                                                                      Net_def_by_taxliabilitiesasoftotalliabilities=`Net _def_by_taxliabilitiesasoftotalliabilities`)


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





# ASK sai to rename vars of liab`s, Dir Salary, cf0, cfo_forex 


# Auditor Name ------------------------------------------------------------

Auditors<-readxl::read_excel("./Prowess/Auditors/auditornames.xlsx") %>%
  mutate(year = lubridate::year(`Slot Date`)) %>% 
  rename(Co_name = `Company Name`,
         Auditor_name = `Auditor Name`)


aud<-readxl::read_excel("./Prowess/Auditors/auditornames_nselisted_Final.xlsx") %>%
  select(-2, -3, -5, -9) %>% 
  rename(Co_name = `Company Name`,
         Auditor_name= `Auditor Name`) %>% 
  group_by(Co_name, year) %>% 
  distinct(Co_name, year, Auditor_name, Big4, big4type)

Auditors<-left_join(Auditors, aud) %>% select(-2, -3) %>% 
  filter(!is.na(Big4))
remove(aud)
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


# Receivables and Inventory -------------------------------------------------------------
Receivables_inventory<-readxl::read_excel("./Prowess/Receivables_Inventories.xlsx")

Receivables_inventory<- Receivables_inventory %>% pivot_longer(.,2:(ncol(Receivables_inventory)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`)

#eeptools::isid(Receivables_inventory, vars = c("Co_name", "year"))

# Equity Ownership ----------------------------------------------------------------------------
Equity_1<-readxl::read_excel("./Prowess/Equity Ownership Listed/eq_2000_2005.xlsx")
Equity_1<-Equity_1 %>%pivot_longer(.,2:ncol(Equity_1), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_shares_num_shares_held=`Total Shares (In nos) - Shares held`,
         Promoters_in_num_shares_held=`Promoters (In nos) - Shares held`,
         Non_promoters_num_promoters_held=`Non-promoters (In nos) - Shares held`,
         Non_prom_inst_num_shares_held=`Non-promoter Institutions (In nos) - Shares held`,
         Non_prom_mf_uti_num_shares_held=`Non-promoter Mutual  Funds/ UTI (In nos) - Shares held`,
         Non_prom_bfi_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In nos) - Shares held`,
         Non_prom_insu_companies_num_shares_held=`Non-promoter Insurance Companies (In nos) - Shares held`,
         Non_prom_fi_banks_num_shares_held=`Non-promoter Financial Institutions & Banks (In nos) - Shares held`,
         Non_prom_cen_state_num_shares_held=`Non-promoter Central & State Government (In nos) - Shares held`,
         Non_prom_fiis_num_shares_held=`Non-promoter FIIs (In nos) - Shares held`,
         Non_prom_vc_num_shares_held=`Non-promoter Venture  Capital Funds (In nos) - Shares held`,
         Non_prom_foreig_vc_num_shares_held=`Non-promoter Foreign Venture Capital (In nos) - Shares held`,
         Non_prom_qfi_inst_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In nos) - Shares held`,
         Other_Institutional_Non_prom_promoters_num_shares_held=`Other Institutional Non-promoters (In nos) - Shares held`,
         Non_prom_non_institutions_num_shares_held=`Non-promoter Non-institutions (In nos) - Shares held`,
         Non_prom_corp_bodies_num_shares_held=`Non-promoter Corporate Bodies (In nos) - Shares held`,
         Non_prom_indi_num_shares_held=`Non-promoter Individuals (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_num_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In nos) - Shares held`,
         Non_prom_qfi_num_shares_held=`Non-promoter Qualified Foreign Investor (In nos) - Shares held`,
         Oth_non_inst_non_prom_num_shares_held=`Other Non-institutional Non-promoters (In nos) - Shares held`,
         Custodians_num_shares_held=`Shares  held  by Custodians (In nos) - Shares held`,
         Total_shares_percent_shares_held=`Total Shares (In %) - Shares held`,
         Promoters_percent_shares_held=`Promoters (In %) - Shares held`,
         Indian_promoters_percent_shares_held=`Indian Promoters (In %) - Shares held`,
         Indian_promoter_Individuals_HUF_percent_shares_held=`Indian Promoter Individuals & HUF (In %) - Shares held`,
         Indian_cen_state_prom_percent_Sha__num_shares_held =`Indian Central & State Govt. Promoters (In %) - Shares held`,
         Indian_prom_corp_bodies_percent_shares_held=`Indian Promoter Corporate Bodies (In %) - Shares held`,
         Indian_prom_FI_Banks_shares_held =`Indian Promoter FIs & Banks (In %) - Shares held`,
         Other_indian_promoters_percent_shares_held=`Other Indian Promoters (In %) - Shares held`,
         Foreign_prom_percent_shares_held=`Foreign Promoters (In %) - Shares held`,
         Foreign_individuals_prom_per_shares_held=`Foreign Individuals (NRIs) Promoters (In %) - Shares held`,
         Foreign_corp_prom_per_shares_held=`Foreign Promoter Corporate Bodies (In %) - Shares held`,
         Foreign_promoter_institutions_num_promoter_held=`Foreign Promoter Institutions (In %) - Shares held`,
         Non_promoter_fiis_percent_num_shares_held=`Non-promoter FIIs (In %) - Shares held`,
         Non_prom_vc_percent_num_shares_held=`Non-promoter Venture  Capital Funds (In %) - Shares held`,
         Non_prom_foreign_vc_percent_num_shares_held=`Non-promoter Foreign Venture Capital (In %) - Shares held`,
         Non_prom_qfi_percent_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In %) - Shares held`,
         Other_Institutional_Non_prom_inst_num_shares_held=`Other Institutional Non-promoters (In %) - Shares held`,
         Non_prom_non_inst_percent_shares_held=`Non-promoter Non-institutions (In %) - Shares held`,
         Non_prom_corp_percent_shares_held=`Non-promoter Corporate Bodies (In %) - Shares held`,
         Non_promoter_individuals_percent_promoter_held=`Non-promoter Individuals (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_upto_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_over_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In %) - Shares held`,
         Non_prom_qfi_percent_shares_held=`Non-promoter Qualified Foreign Investor (In %) - Shares held`,
         Oth_non_inst_non_prom_percent_shares_held=`Other Non-institutional Non-promoters (In %) - Shares held`,
         Custodians_percent_shares_held=`Shares  held  by Custodians (In %) - Shares held`,
         Indian_promoters_num_shares_held=`Indian Promoters (In nos) - Shares held`,
         Indian_promoter_Individuals_HUF_num_shares_held=`Indian Promoter Individuals & HUF (In nos) - Shares held`,
         Indian_cen_state_prom_num_shares_held=`Indian Central & State Govt. Promoters (In nos) - Shares held`,
         Indian_prom_corp_bodies_num_shares_held=`Indian Promoter Corporate Bodies (In nos) - Shares held`,
         Indian_prom_FI_Banks_num_shares_held =`Indian Promoter FIs & Banks (In nos) - Shares held`,
         Other_indian_promoters_num_indian_held=`Other Indian Promoters (In nos) - Shares held`,
         Foreign_prom_num_shares_held=`Foreign Promoters (In nos) - Shares held`,
         Foreign_individuals_prom__nos_shares_held=`Foreign Individuals (NRIs) Promoters (In nos) - Shares held`,
         Foreign_corp_prom_nos_shares_held=`Foreign Promoter Corporate Bodies (In nos) - Shares held`,
         Foreign_prom_inst_num_shares_held=`Foreign Promoter Institutions (In nos) - Shares held`,
         Prom_qfi_num_shares_held=`Promoter Qualified Foreign Investor (In nos) - Shares held`,
         Other_foreign_promoters_num_foreign_held_nos=`Other Foreign Promoters (In nos) - Shares held`,
         Persons_act_concert_prom_num_shares_held=`Persons acting in concert as promoters (In nos) - Shares held`,
         Non_prom_insu_percent_shares_held=`Non-promoter Insurance Companies (In %) - Shares held`,
         Non_prom_fi_banks_percent_shares_held=`Non-promoter Financial Institutions & Banks (In %) - Shares held`,
         Non_prom_cen_state_percent_shares_held=`Non-promoter Central & State Government (In %) - Shares held`,
         Promoter_Qualified_Foreign_Investor_per_shares_held= `Promoter Qualified Foreign Investor (In %) - Shares held`,
         Other_foreign_promoters_num_foreign_held_per=`Other Foreign Promoters (In %) - Shares held`,
         Persons_act_concert_prom_percent_shares_held=`Persons acting in concert as promoters (In %) - Shares held`,
         Non_promoters_percent_shares_held=`Non-promoters (In %) - Shares held`,
         Non_prom_inst_percent_shares_held=`Non-promoter Institutions (In %) - Shares held`,
         Non_prom_mf_uti_percent_shares_held=`Non-promoter Mutual  Funds/ UTI (In %) - Shares held`,
         Non_prom_bfi_percent_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In %) - Shares held`)
Equity_2<-readxl::read_excel("./Prowess/Equity Ownership Listed/eq_2006_2010.xlsx")
Equity_2<-Equity_2 %>%pivot_longer(.,2:ncol(Equity_2), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_shares_num_shares_held=`Total Shares (In nos) - Shares held`,
         Promoters_in_num_shares_held=`Promoters (In nos) - Shares held`,
         Non_promoters_num_promoters_held=`Non-promoters (In nos) - Shares held`,
         Non_prom_inst_num_shares_held=`Non-promoter Institutions (In nos) - Shares held`,
         Non_prom_mf_uti_num_shares_held=`Non-promoter Mutual  Funds/ UTI (In nos) - Shares held`,
         Non_prom_bfi_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In nos) - Shares held`,
         Non_prom_insu_companies_num_shares_held=`Non-promoter Insurance Companies (In nos) - Shares held`,
         Non_prom_fi_banks_num_shares_held=`Non-promoter Financial Institutions & Banks (In nos) - Shares held`,
         Non_prom_cen_state_num_shares_held=`Non-promoter Central & State Government (In nos) - Shares held`,
         Non_prom_fiis_num_shares_held=`Non-promoter FIIs (In nos) - Shares held`,
         Non_prom_vc_num_shares_held=`Non-promoter Venture  Capital Funds (In nos) - Shares held`,
         Non_prom_foreig_vc_num_shares_held=`Non-promoter Foreign Venture Capital (In nos) - Shares held`,
         Non_prom_qfi_inst_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In nos) - Shares held`,
         Other_Institutional_Non_prom_promoters_num_shares_held=`Other Institutional Non-promoters (In nos) - Shares held`,
         Non_prom_non_institutions_num_shares_held=`Non-promoter Non-institutions (In nos) - Shares held`,
         Non_prom_corp_bodies_num_shares_held=`Non-promoter Corporate Bodies (In nos) - Shares held`,
         Non_prom_indi_num_shares_held=`Non-promoter Individuals (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_num_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In nos) - Shares held`,
         Non_prom_qfi_num_shares_held=`Non-promoter Qualified Foreign Investor (In nos) - Shares held`,
         Oth_non_inst_non_prom_num_shares_held=`Other Non-institutional Non-promoters (In nos) - Shares held`,
         Custodians_num_shares_held=`Shares  held  by Custodians (In nos) - Shares held`,
         Total_shares_percent_shares_held=`Total Shares (In %) - Shares held`,
         Promoters_percent_shares_held=`Promoters (In %) - Shares held`,
         Indian_promoters_percent_shares_held=`Indian Promoters (In %) - Shares held`,
         Indian_promoter_Individuals_HUF_percent_shares_held=`Indian Promoter Individuals & HUF (In %) - Shares held`,
         Indian_cen_state_prom_percent_Sha__num_shares_held =`Indian Central & State Govt. Promoters (In %) - Shares held`,
         Indian_prom_corp_bodies_percent_shares_held=`Indian Promoter Corporate Bodies (In %) - Shares held`,
         Indian_prom_FI_Banks_shares_held =`Indian Promoter FIs & Banks (In %) - Shares held`,
         Other_indian_promoters_percent_shares_held=`Other Indian Promoters (In %) - Shares held`,
         Foreign_prom_percent_shares_held=`Foreign Promoters (In %) - Shares held`,
         Foreign_individuals_prom_per_shares_held=`Foreign Individuals (NRIs) Promoters (In %) - Shares held`,
         Foreign_corp_prom_per_shares_held=`Foreign Promoter Corporate Bodies (In %) - Shares held`,
         Foreign_promoter_institutions_num_promoter_held=`Foreign Promoter Institutions (In %) - Shares held`,
         Non_promoter_fiis_percent_num_shares_held=`Non-promoter FIIs (In %) - Shares held`,
         Non_prom_vc_percent_num_shares_held=`Non-promoter Venture  Capital Funds (In %) - Shares held`,
         Non_prom_foreign_vc_percent_num_shares_held=`Non-promoter Foreign Venture Capital (In %) - Shares held`,
         Non_prom_qfi_percent_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In %) - Shares held`,
         Other_Institutional_Non_prom_inst_num_shares_held=`Other Institutional Non-promoters (In %) - Shares held`,
         Non_prom_non_inst_percent_shares_held=`Non-promoter Non-institutions (In %) - Shares held`,
         Non_prom_corp_percent_shares_held=`Non-promoter Corporate Bodies (In %) - Shares held`,
         Non_promoter_individuals_percent_promoter_held=`Non-promoter Individuals (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_upto_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_over_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In %) - Shares held`,
         Non_prom_qfi_percent_shares_held=`Non-promoter Qualified Foreign Investor (In %) - Shares held`,
         Oth_non_inst_non_prom_percent_shares_held=`Other Non-institutional Non-promoters (In %) - Shares held`,
         Custodians_percent_shares_held=`Shares  held  by Custodians (In %) - Shares held`,
         Indian_promoters_num_shares_held=`Indian Promoters (In nos) - Shares held`,
         Indian_promoter_Individuals_HUF_num_shares_held=`Indian Promoter Individuals & HUF (In nos) - Shares held`,
         Indian_cen_state_prom_num_shares_held=`Indian Central & State Govt. Promoters (In nos) - Shares held`,
         Indian_prom_corp_bodies_num_shares_held=`Indian Promoter Corporate Bodies (In nos) - Shares held`,
         Indian_prom_FI_Banks_num_shares_held =`Indian Promoter FIs & Banks (In nos) - Shares held`,
         Other_indian_promoters_num_indian_held=`Other Indian Promoters (In nos) - Shares held`,
         Foreign_prom_num_shares_held=`Foreign Promoters (In nos) - Shares held`,
         Foreign_individuals_prom__nos_shares_held=`Foreign Individuals (NRIs) Promoters (In nos) - Shares held`,
         Foreign_corp_prom_nos_shares_held=`Foreign Promoter Corporate Bodies (In nos) - Shares held`,
         Foreign_prom_inst_num_shares_held=`Foreign Promoter Institutions (In nos) - Shares held`,
         Prom_qfi_num_shares_held=`Promoter Qualified Foreign Investor (In nos) - Shares held`,
         Other_foreign_promoters_num_foreign_held_nos=`Other Foreign Promoters (In nos) - Shares held`,
         Persons_act_concert_prom_num_shares_held=`Persons acting in concert as promoters (In nos) - Shares held`,
         Non_prom_insu_percent_shares_held=`Non-promoter Insurance Companies (In %) - Shares held`,
         Non_prom_fi_banks_percent_shares_held=`Non-promoter Financial Institutions & Banks (In %) - Shares held`,
         Non_prom_cen_state_percent_shares_held=`Non-promoter Central & State Government (In %) - Shares held`,
         Promoter_Qualified_Foreign_Investor_per_shares_held= `Promoter Qualified Foreign Investor (In %) - Shares held`,
         Other_foreign_promoters_num_foreign_held_per=`Other Foreign Promoters (In %) - Shares held`,
         Persons_act_concert_prom_percent_shares_held=`Persons acting in concert as promoters (In %) - Shares held`,
         Non_promoters_percent_shares_held=`Non-promoters (In %) - Shares held`,
         Non_prom_inst_percent_shares_held=`Non-promoter Institutions (In %) - Shares held`,
         Non_prom_mf_uti_percent_shares_held=`Non-promoter Mutual  Funds/ UTI (In %) - Shares held`,
         Non_prom_bfi_percent_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In %) - Shares held`)
Equity_3<-readxl::read_excel("./Prowess/Equity Ownership Listed/eq_2011_2015.xlsx")
Equity_3<-Equity_3 %>%pivot_longer(.,2:ncol(Equity_3), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_shares_num_shares_held=`Total Shares (In nos) - Shares held`,
         Promoters_in_num_shares_held=`Promoters (In nos) - Shares held`,
         Non_promoters_num_promoters_held=`Non-promoters (In nos) - Shares held`,
         Non_prom_inst_num_shares_held=`Non-promoter Institutions (In nos) - Shares held`,
         Non_prom_mf_uti_num_shares_held=`Non-promoter Mutual  Funds/ UTI (In nos) - Shares held`,
         Non_prom_bfi_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In nos) - Shares held`,
         Non_prom_insu_companies_num_shares_held=`Non-promoter Insurance Companies (In nos) - Shares held`,
         Non_prom_fi_banks_num_shares_held=`Non-promoter Financial Institutions & Banks (In nos) - Shares held`,
         Non_prom_cen_state_num_shares_held=`Non-promoter Central & State Government (In nos) - Shares held`,
         Non_prom_fiis_num_shares_held=`Non-promoter FIIs (In nos) - Shares held`,
         Non_prom_vc_num_shares_held=`Non-promoter Venture  Capital Funds (In nos) - Shares held`,
         Non_prom_foreig_vc_num_shares_held=`Non-promoter Foreign Venture Capital (In nos) - Shares held`,
         Non_prom_qfi_inst_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In nos) - Shares held`,
         Other_Institutional_Non_prom_promoters_num_shares_held=`Other Institutional Non-promoters (In nos) - Shares held`,
         Non_prom_non_institutions_num_shares_held=`Non-promoter Non-institutions (In nos) - Shares held`,
         Non_prom_corp_bodies_num_shares_held=`Non-promoter Corporate Bodies (In nos) - Shares held`,
         Non_prom_indi_num_shares_held=`Non-promoter Individuals (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_num_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In nos) - Shares held`,
         Non_prom_qfi_num_shares_held=`Non-promoter Qualified Foreign Investor (In nos) - Shares held`,
         Oth_non_inst_non_prom_num_shares_held=`Other Non-institutional Non-promoters (In nos) - Shares held`,
         Custodians_num_shares_held=`Shares  held  by Custodians (In nos) - Shares held`,
         Total_shares_percent_shares_held=`Total Shares (In %) - Shares held`,
         Promoters_percent_shares_held=`Promoters (In %) - Shares held`,
         Indian_promoters_percent_shares_held=`Indian Promoters (In %) - Shares held`,
         Indian_promoter_Individuals_HUF_percent_shares_held=`Indian Promoter Individuals & HUF (In %) - Shares held`,
         Indian_cen_state_prom_percent_Sha__num_shares_held =`Indian Central & State Govt. Promoters (In %) - Shares held`,
         Indian_prom_corp_bodies_percent_shares_held=`Indian Promoter Corporate Bodies (In %) - Shares held`,
         Indian_prom_FI_Banks_shares_held =`Indian Promoter FIs & Banks (In %) - Shares held`,
         Other_indian_promoters_percent_shares_held=`Other Indian Promoters (In %) - Shares held`,
         Foreign_prom_percent_shares_held=`Foreign Promoters (In %) - Shares held`,
         Foreign_individuals_prom_per_shares_held=`Foreign Individuals (NRIs) Promoters (In %) - Shares held`,
         Foreign_corp_prom_per_shares_held=`Foreign Promoter Corporate Bodies (In %) - Shares held`,
         Foreign_promoter_institutions_num_promoter_held=`Foreign Promoter Institutions (In %) - Shares held`,
         Non_promoter_fiis_percent_num_shares_held=`Non-promoter FIIs (In %) - Shares held`,
         Non_prom_vc_percent_num_shares_held=`Non-promoter Venture  Capital Funds (In %) - Shares held`,
         Non_prom_foreign_vc_percent_num_shares_held=`Non-promoter Foreign Venture Capital (In %) - Shares held`,
         Non_prom_qfi_percent_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In %) - Shares held`,
         Other_Institutional_Non_prom_inst_num_shares_held=`Other Institutional Non-promoters (In %) - Shares held`,
         Non_prom_non_inst_percent_shares_held=`Non-promoter Non-institutions (In %) - Shares held`,
         Non_prom_corp_percent_shares_held=`Non-promoter Corporate Bodies (In %) - Shares held`,
         Non_promoter_individuals_percent_promoter_held=`Non-promoter Individuals (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_upto_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_over_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In %) - Shares held`,
         Non_prom_qfi_percent_shares_held=`Non-promoter Qualified Foreign Investor (In %) - Shares held`,
         Oth_non_inst_non_prom_percent_shares_held=`Other Non-institutional Non-promoters (In %) - Shares held`,
         Custodians_percent_shares_held=`Shares  held  by Custodians (In %) - Shares held`,
         Indian_promoters_num_shares_held=`Indian Promoters (In nos) - Shares held`,
         Indian_promoter_Individuals_HUF_num_shares_held=`Indian Promoter Individuals & HUF (In nos) - Shares held`,
         Indian_cen_state_prom_num_shares_held=`Indian Central & State Govt. Promoters (In nos) - Shares held`,
         Indian_prom_corp_bodies_num_shares_held=`Indian Promoter Corporate Bodies (In nos) - Shares held`,
         Indian_prom_FI_Banks_num_shares_held =`Indian Promoter FIs & Banks (In nos) - Shares held`,
         Other_indian_promoters_num_indian_held=`Other Indian Promoters (In nos) - Shares held`,
         Foreign_prom_num_shares_held=`Foreign Promoters (In nos) - Shares held`,
         Foreign_individuals_prom__nos_shares_held=`Foreign Individuals (NRIs) Promoters (In nos) - Shares held`,
         Foreign_corp_prom_nos_shares_held=`Foreign Promoter Corporate Bodies (In nos) - Shares held`,
         Foreign_prom_inst_num_shares_held=`Foreign Promoter Institutions (In nos) - Shares held`,
         Prom_qfi_num_shares_held=`Promoter Qualified Foreign Investor (In nos) - Shares held`,
         Other_foreign_promoters_num_foreign_held_nos=`Other Foreign Promoters (In nos) - Shares held`,
         Persons_act_concert_prom_num_shares_held=`Persons acting in concert as promoters (In nos) - Shares held`,
         Non_prom_insu_percent_shares_held=`Non-promoter Insurance Companies (In %) - Shares held`,
         Non_prom_fi_banks_percent_shares_held=`Non-promoter Financial Institutions & Banks (In %) - Shares held`,
         Non_prom_cen_state_percent_shares_held=`Non-promoter Central & State Government (In %) - Shares held`,
         Promoter_Qualified_Foreign_Investor_per_shares_held= `Promoter Qualified Foreign Investor (In %) - Shares held`,
         Other_foreign_promoters_num_foreign_held_per=`Other Foreign Promoters (In %) - Shares held`,
         Persons_act_concert_prom_percent_shares_held=`Persons acting in concert as promoters (In %) - Shares held`,
         Non_promoters_percent_shares_held=`Non-promoters (In %) - Shares held`,
         Non_prom_inst_percent_shares_held=`Non-promoter Institutions (In %) - Shares held`,
         Non_prom_mf_uti_percent_shares_held=`Non-promoter Mutual  Funds/ UTI (In %) - Shares held`,
         Non_prom_bfi_percent_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In %) - Shares held`)
Equity_4<-readxl::read_excel("./Prowess/Equity Ownership Listed/eq_2016_2020.xlsx")
Equity_4<-Equity_4 %>%pivot_longer(.,2:ncol(Equity_4), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_shares_num_shares_held=`Total Shares (In nos) - Shares held`,
         Promoters_in_num_shares_held=`Promoters (In nos) - Shares held`,
         Non_promoters_num_promoters_held=`Non-promoters (In nos) - Shares held`,
         Non_prom_inst_num_shares_held=`Non-promoter Institutions (In nos) - Shares held`,
         Non_prom_mf_uti_num_shares_held=`Non-promoter Mutual  Funds/ UTI (In nos) - Shares held`,
         Non_prom_bfi_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In nos) - Shares held`,
         Non_prom_insu_companies_num_shares_held=`Non-promoter Insurance Companies (In nos) - Shares held`,
         Non_prom_fi_banks_num_shares_held=`Non-promoter Financial Institutions & Banks (In nos) - Shares held`,
         Non_prom_cen_state_num_shares_held=`Non-promoter Central & State Government (In nos) - Shares held`,
         Non_prom_fiis_num_shares_held=`Non-promoter FIIs (In nos) - Shares held`,
         Non_prom_vc_num_shares_held=`Non-promoter Venture  Capital Funds (In nos) - Shares held`,
         Non_prom_foreig_vc_num_shares_held=`Non-promoter Foreign Venture Capital (In nos) - Shares held`,
         Non_prom_qfi_inst_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In nos) - Shares held`,
         Other_Institutional_Non_prom_promoters_num_shares_held=`Other Institutional Non-promoters (In nos) - Shares held`,
         Non_prom_non_institutions_num_shares_held=`Non-promoter Non-institutions (In nos) - Shares held`,
         Non_prom_corp_bodies_num_shares_held=`Non-promoter Corporate Bodies (In nos) - Shares held`,
         Non_prom_indi_num_shares_held=`Non-promoter Individuals (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_num_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In nos) - Shares held`,
         Non_prom_qfi_num_shares_held=`Non-promoter Qualified Foreign Investor (In nos) - Shares held`,
         Oth_non_inst_non_prom_num_shares_held=`Other Non-institutional Non-promoters (In nos) - Shares held`,
         Custodians_num_shares_held=`Shares  held  by Custodians (In nos) - Shares held`,
         Total_shares_percent_shares_held=`Total Shares (In %) - Shares held`,
         Promoters_percent_shares_held=`Promoters (In %) - Shares held`,
         Indian_promoters_percent_shares_held=`Indian Promoters (In %) - Shares held`,
         Indian_promoter_Individuals_HUF_percent_shares_held=`Indian Promoter Individuals & HUF (In %) - Shares held`,
         Indian_cen_state_prom_percent_Sha__num_shares_held =`Indian Central & State Govt. Promoters (In %) - Shares held`,
         Indian_prom_corp_bodies_percent_shares_held=`Indian Promoter Corporate Bodies (In %) - Shares held`,
         Indian_prom_FI_Banks_shares_held =`Indian Promoter FIs & Banks (In %) - Shares held`,
         Other_indian_promoters_percent_shares_held=`Other Indian Promoters (In %) - Shares held`,
         Foreign_prom_percent_shares_held=`Foreign Promoters (In %) - Shares held`,
         Foreign_individuals_prom_per_shares_held=`Foreign Individuals (NRIs) Promoters (In %) - Shares held`,
         Foreign_corp_prom_per_shares_held=`Foreign Promoter Corporate Bodies (In %) - Shares held`,
         Foreign_promoter_institutions_num_promoter_held=`Foreign Promoter Institutions (In %) - Shares held`,
         Non_promoter_fiis_percent_num_shares_held=`Non-promoter FIIs (In %) - Shares held`,
         Non_prom_vc_percent_num_shares_held=`Non-promoter Venture  Capital Funds (In %) - Shares held`,
         Non_prom_foreign_vc_percent_num_shares_held=`Non-promoter Foreign Venture Capital (In %) - Shares held`,
         Non_prom_qfi_percent_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In %) - Shares held`,
         Other_Institutional_Non_prom_inst_num_shares_held=`Other Institutional Non-promoters (In %) - Shares held`,
         Non_prom_non_inst_percent_shares_held=`Non-promoter Non-institutions (In %) - Shares held`,
         Non_prom_corp_percent_shares_held=`Non-promoter Corporate Bodies (In %) - Shares held`,
         Non_promoter_individuals_percent_promoter_held=`Non-promoter Individuals (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_upto_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_over_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In %) - Shares held`,
         Non_prom_qfi_percent_shares_held=`Non-promoter Qualified Foreign Investor (In %) - Shares held`,
         Oth_non_inst_non_prom_percent_shares_held=`Other Non-institutional Non-promoters (In %) - Shares held`,
         Custodians_percent_shares_held=`Shares  held  by Custodians (In %) - Shares held`,
         Indian_promoters_num_shares_held=`Indian Promoters (In nos) - Shares held`,
         Indian_promoter_Individuals_HUF_num_shares_held=`Indian Promoter Individuals & HUF (In nos) - Shares held`,
         Indian_cen_state_prom_num_shares_held=`Indian Central & State Govt. Promoters (In nos) - Shares held`,
         Indian_prom_corp_bodies_num_shares_held=`Indian Promoter Corporate Bodies (In nos) - Shares held`,
         Indian_prom_FI_Banks_num_shares_held =`Indian Promoter FIs & Banks (In nos) - Shares held`,
         Other_indian_promoters_num_indian_held=`Other Indian Promoters (In nos) - Shares held`,
         Foreign_prom_num_shares_held=`Foreign Promoters (In nos) - Shares held`,
         Foreign_individuals_prom__nos_shares_held=`Foreign Individuals (NRIs) Promoters (In nos) - Shares held`,
         Foreign_corp_prom_nos_shares_held=`Foreign Promoter Corporate Bodies (In nos) - Shares held`,
         Foreign_prom_inst_num_shares_held=`Foreign Promoter Institutions (In nos) - Shares held`,
         Prom_qfi_num_shares_held=`Promoter Qualified Foreign Investor (In nos) - Shares held`,
         Other_foreign_promoters_num_foreign_held_nos=`Other Foreign Promoters (In nos) - Shares held`,
         Persons_act_concert_prom_num_shares_held=`Persons acting in concert as promoters (In nos) - Shares held`,
         Non_prom_insu_percent_shares_held=`Non-promoter Insurance Companies (In %) - Shares held`,
         Non_prom_fi_banks_percent_shares_held=`Non-promoter Financial Institutions & Banks (In %) - Shares held`,
         Non_prom_cen_state_percent_shares_held=`Non-promoter Central & State Government (In %) - Shares held`,
         Promoter_Qualified_Foreign_Investor_per_shares_held= `Promoter Qualified Foreign Investor (In %) - Shares held`,
         Other_foreign_promoters_num_foreign_held_per=`Other Foreign Promoters (In %) - Shares held`,
         Persons_act_concert_prom_percent_shares_held=`Persons acting in concert as promoters (In %) - Shares held`,
         Non_promoters_percent_shares_held=`Non-promoters (In %) - Shares held`,
         Non_prom_inst_percent_shares_held=`Non-promoter Institutions (In %) - Shares held`,
         Non_prom_mf_uti_percent_shares_held=`Non-promoter Mutual  Funds/ UTI (In %) - Shares held`,
         Non_prom_bfi_percent_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In %) - Shares held`)
Equity<-bind_rows(Equity_1,Equity_2,Equity_3,Equity_4)
remove(Equity_1,Equity_2,Equity_3,Equity_4)
#eeptools::isid(Equity, vars = c("Co_name", "year"))



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
  left_join(., Equity,by=c("Co_name","year")) %>% 
  left_join(., NSE_indicators, by= c("Co_name")) %>% 
  mutate(unlisted=0)


Aud_NSE<-left_join(Auditors, NSE_indicators, by= c("Co_name"))                         

Aud_NSE<-Aud_NSE %>% filter(Indust_type!=3) #dups verified and then filtered to remove banks. wait until confirmation to merge with FV

remove(FV_2, assets_1, Liabilities_deposits, capital, workingcapital, rmconsumed, empbranches, Dir_salary, cfo, cfo_forex,
       promhol, return_ratios, Receivables_inventory, Equity)







# Merging FV to Auditors --------------------------------------------------
FV<-left_join(FV, Auditors)







# Profit 1 unlist Cleaning ---------------------------------------------------------
Profit_un_1<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Profit/1.xlsx")

Profit_un_1 <- Profit_un_1 %>% pivot_longer(.,2:(ncol(Profit_un_1)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Profit_after_tax = `Profit after tax`,
         PBDITA  = `PBDITA`,
         PBPT = `PBPT`,
         PBT = `PBT`,
         PBIT_net = `PBIT net of P&E&OI&FI`,
         PBIT = `PBIT`,
         Cash_profit = `Cash profit`)


# Profit 2 unlist Cleaning ---------------------------------------------------------
Profit_un_2<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Profit/2.xlsx")

Profit_un_2<- Profit_un_2 %>% pivot_longer(.,2:(ncol(Profit_un_2)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         PBPT_net_fin = `PBPT net of P&E&OI to inc fin serv`,
         PBPT_net_emp = `PBPT net of P&E&OI per employee`,
         Cash_profit_net_PE = `Cash profit net of P&E`,
         PAT= `PAT net of P&E`,
         OP_profit_NFC= `Operating  profit of non-financial companies`,
         OP_profit_FC = `Operating profit of financial companies`)

# Profit 3 unlist Cleaning ---------------------------------------------------------
Profit_un_3<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Profit/3.xlsx")

Profit_un_3 <- Profit_un_3 %>% pivot_longer(.,2:(ncol(Profit_un_3)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         PAT_continuing= `PAT from continuing ops as % of income from continuing ops`,
         PAT_discontinuing= `PAT discont ops as % of income from disocont ops`,
         PAT_from_continuing= `Profit after tax from continuing operations`,
         P_L_on_discontinuing= `Profit/loss after tax on discontinuing operations`,
         NPBT_Extra_Ordinary = `Net profit before tax and extra ordinary items`)

Profit_un= left_join(Profit_un_2, Profit_un_1, by= c("Co_name","year")) %>% 
  left_join(.,Profit_un_3,by= c("Co_name","year"))
remove(Profit_un_1, Profit_un_2,Profit_un_3)
#eeptools::isid(Profit_un, vars = c("Co_name", "year"))

# Income 1 unlist Cleaning -------------------------------------------------------
Income_un_1<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Income/1.xlsx")

Income_un_1 <- Income_un_1 %>% mutate(`Total income2000`= as.numeric(case_when(
  `Total income2000`=="`" ~ NA_character_,
  TRUE ~ `Total income2000`))) %>% 
  pivot_longer(.,2:(ncol(Income_un_1)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,        
         TI = 'Total income',
         Sales =  'Sales',
         Income_Financial =  'Income from financial services',
         Income_non_fin=`Income from non-financial services`,
         Other_Income = 'Other income',
         Industrial_sales = 'Industrial sales',
         Sales_returns = 'Sales returns',
         Trade_discount = 'Trade discount')

# Income 2 unlist Cleaning -------------------------------------------------------
Income_un_2<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Income/2.xlsx")

Income_un_2 <- Income_un_2 %>% pivot_longer(.,2:(ncol(Income_un_2)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         FBFSI =`Fee based financial services income`,
         FundBFSI = `Fund based financial services income`,
         Other_fiancial = `Other financial services income`,
         Expenses_recovered = `Expenses recovered`,
         LDCR =`Liquidated damages and claims received`,
         Amortaisation = `Amortisation of deferred income`,
         Government_Grant=`Revenue government grant`,
         Miscellaneous=`Miscellaneous income`)
# Income 3 unlist Cleaning -------------------------------------------------------
Income_un_3<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Income/3.xlsx")
Income_un_3 <- Income_un_3 %>% pivot_longer(.,2:(ncol(Income_un_3)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         PP_Extra_Ordinary = `Prior period and extra-ordinary income`,
         Prior_period=`Prior period income`,
         Extra_ordinary=`Extra-ordinary income`,
         Capitalised=`Income capitalised`,
         Interest_Capitalised=`Interest income capitalised`,
         DRE=`Income transferred to DRE`)
# Income 4 unlist Cleaning -------------------------------------------------------
Income_un_4<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Income/4.xlsx")
Income_un_4 <- Income_un_4 %>% pivot_longer(.,2:(ncol(Income_un_4)), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>%
  pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         TDS = `Tax deducted at source (TDS)`,
         Internal_Transfers=`Internal transfers`,
         TI_Extra_Ordinary=`Total income net of prior period and extra-ordinary income`,
         Sales_CIS=`Sales and change in stocks`,
         Net_Sales=`Net sales`,
         Sales_NFA=`Sales / Net fixed assets`)

Income_un= left_join(Income_un_4,Income_un_2,by= c("Co_name","year")) %>% 
  left_join(.,Income_un_1,by=c("Co_name","year")) %>% 
  left_join(.,Income_un_3,by=c("Co_name","year"))
remove(Income_un_1, Income_un_2,Income_un_3,Income_un_4)
#eeptools::isid(Income_un, vars = c("Co_name", "year"))

# Stock Price and Capital Changes unlist -----------------------------------------
Adjust_close_price<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Adjusted Closing Price.xlsx") %>%  
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Adj_closing_price") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Cash_EPS<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Cash EPS.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Cash_EPS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Cons_cash_EPS<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Consolidated Cash EPS.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Cons_cash_EPS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Cons_EPS<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Consolidated EPS.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Cons_EPS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

EPS<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/EPS AND PB.xlsx") %>% 
  select(1,starts_with("EPS")) %>% 
  tidyr::pivot_longer(starts_with("EPS"), names_to = "year", values_to = "EPS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

PB<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/EPS AND PB.xlsx") %>% 
  select(1,starts_with("P/B")) %>% 
  tidyr::pivot_longer(starts_with("P/B"), names_to = "year", values_to = "PB") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Face_value<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Face Value.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Face_value") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Mkt_capital<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Market Capitalisation.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Mkt_capital") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

PE<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/PE.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "P_E") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Shares_OS<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Shares Outstanding.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Shares_OS") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

BV_per_share<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("BV per Share")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "BV_per_share") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Yield<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Yield")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Yield") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Turnover<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Turnover")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Turnover") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Shares_traded<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Shares traded")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Shares_traded") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Weighted_avg_price<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Weighted")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Weighted_avg_price") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Transactions_num<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Number of Transactions")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Transactions_num") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Shares_deliverable<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Shares deliverable2")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Shares_deliverable") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Shares_deliverable_asp_traded<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Shares deliverable as")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Shares_deliverable_asp_traded") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

EV<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Enterprise value2")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "EV") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

Mkt_capital_by_EV<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Market Capitalisation")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Mkt_capital_by_EV") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L)))%>% rename(Co_name= `Company Name`)

EV_by_PBDITA<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Stock Prices and capital Changes NSE.xlsx") %>% 
  select(1,starts_with("Enterprise Value / PBDITA")) %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "EV_by_PBDITA") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Total_returns<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Stock Prices and capital Changes/Total Returns.xlsx") %>% 
  tidyr::pivot_longer(2:22, names_to = "year", values_to = "Total_returns") %>% 
  mutate(year=as.double(stringr::str_sub(year, start = -4L))) %>% rename(Co_name= `Company Name`)

Stock_prices_capital_changes_un<-left_join(BV_per_share, Adjust_close_price, by=c("Co_name", "year")) %>% 
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

#eeptools::isid(Stock_prices_capital_changes_un, vars = c("Co_name", "year"))


# Merging Income, Profit, Stock Price and Capital Changes -----------------

FV_1_un<-left_join(Income_un,Profit_un, by= c("Co_name", "year")) %>% 
  left_join(., Stock_prices_capital_changes_un, by= c("Co_name", "year"))

#eeptools::isid(FV_1_un, vars = c("Co_name", "year"))

remove(Income_un, Profit_un, Stock_prices_capital_changes_un)


# Change in Stock unlist ------------------------------------------------

ChangeinStock_un<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Change_in_stock_unlisted_21yrs.xlsx")

ChangeinStock_un <- ChangeinStock_un %>% pivot_longer(.,2:ncol(ChangeinStock_un), names_to = "vars", values_to = "vals") %>% 
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

#eeptools::isid(ChangeinStock_un, vars = c("Co_name", "year"))
# Contingent Liabilities unlist --------------------------------------------------

# Liabilities

CntgtLiabilities_un<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Contingentliabilities_1_unlisted_21yrs.xlsx")
Guarantee_un<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Contingentliabilities_2_unlisted_21yrs.xlsx")
Committments_un<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Contingentliabilities_3_unlisted_21yrs.xlsx")

# Liabilities

CntgtLiabilities_un <- CntgtLiabilities_un %>% pivot_longer(.,2:ncol(CntgtLiabilities_un), names_to = "vars", values_to = "vals") %>% 
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
Guarantee_un<- Guarantee_un%>% pivot_longer(.,2:ncol(Guarantee_un), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Dispute_claims =`Disputed claims or others`,
         Dispute_licensefees=`Disputed licence fees`,
         Dispute_leaserentals=`Disputed lease rentals`,
         Otherclaimsdisputed=`Other claims disputed`,
         Gnt_n_cntrguarantee=`Guarantees and counter-guarantees`,
         Gnt= `Guarantees`,
         Guarantee_grpcomp=`Guarantee for group companies`,
         Guarantee_in_India= `Guarantee given in India (for finance companies)`,
         Guarantee_outside=`Guarantee given outside India (for finance companies)`,
         Cntrguarantee_bycomp=`Counter guarantees by company`,
         Cntrguarantee_forgrpcomp=`Counter guarantees for group companies`)
# Committments
Committments_un<- Committments_un %>% pivot_longer(.,2:ncol(Committments_un), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         CntgtLiabOther =`Other contingent liabilities`,
         Arrs_dividend=`Arrears of preference dividend`,
         Empdues=`Unprovided employee dues`,
         Liab_paidupshares=`Liabilities of un-called and partly paidup shares & debentures`,
         Liab_underwrtoblg=`Liabilities of underwriting obligation`,
         CntgtLiabMisc=`Other miscellaneous contingent liabilities`,
         Rnd_expensestot=`Research & development expenses (capital & current account)`,
         Rnd_expensescapacct=`Research & development expenses - capital account`,
         Rnd_expensescurracct=`Research & development expenses - current account`,
         Cmmts=`Commitments`,
         Cmmts_on_capacct= `Commitment on capital account`,
         Cmmts_on_revacct =`Commitment on other/revenue account`)

CntgtLiabilities_un<-left_join(CntgtLiabilities_un, Guarantee_un, by= c("Co_name","year")) %>%
  left_join(.,Committments_un, by=c("Co_name","year"))

remove(Guarantee_un,Committments_un)

# CSR_Disclosures unlist---------------------------------------------------------

CSR_Disclosures_un<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/CSR_Disclosures_unlisted_21yrs.xlsx")

CSR_Disclosures_un <- CSR_Disclosures_un %>% pivot_longer(.,2:ncol(CSR_Disclosures_un), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Avg_netprofit=`Average net profit for last three financial year`,
         CSR_expend_incurred=`CSR expenditure to be incurred as per Companies Act 2013`,
         Tot_amtspent_CSR=`Total amount spent on CSR activities during the year`,
         Tot_amtspent_CSR_curryear=`Of which: Amount spent on CSR actvities for the current year`,
         Tot_amtunspent_CSR=`Total CSR amount unspent as on year end`,
         Tot_amtunspent_CSR_curryear=`CSR amount unspent pertaining to the current year`)
# Expenses unlist ----------------------------------------------------------
Expenses1_10_1<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_1-10_unlist_1.xlsx")
Expenses1_10_2<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_1-10_unlist_2.xlsx")
Expenses1_10_un<-bind_rows(Expenses1_10_1,Expenses1_10_2)
remove(Expenses1_10_1,Expenses1_10_2)
Expenses1_10_un <- Expenses1_10_un %>% pivot_longer(.,2:ncol(Expenses1_10_un), names_to = "vars", values_to = "vals") %>% 
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

Expenses11_20_1<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_11-20_unlist_1.xlsx")
Expenses11_20_2<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_11-20_unlist_2.xlsx")
Expenses11_20_un<-bind_rows(Expenses11_20_1,Expenses11_20_2)
remove(Expenses11_20_1,Expenses11_20_2)
Expenses11_20_un <- Expenses11_20_un %>% pivot_longer(.,2:ncol(Expenses11_20_un), names_to = "vars", values_to = "vals") %>% 
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

Expenses21_32_1<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_21-32_unlist_1.xlsx")
Expenses21_32_2<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_21-32_unlist_2.xlsx")
Expenses21_32_un<-bind_rows(Expenses21_32_1,Expenses21_32_2)
remove(Expenses21_32_1,Expenses21_32_2)
Expenses21_32_un <- Expenses21_32_un %>% pivot_longer(.,2:ncol(Expenses21_32_un), names_to = "vars", values_to = "vals") %>% 
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
Expenses_Provns_Deprc_1<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_Provisions_Depreciation_unlist_1.xlsx")
Expenses_Provns_Deprc_2<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_Provisions_Depreciation_unlist_2.xlsx")
Expenses_Provns_Deprc_un<- bind_rows(Expenses_Provns_Deprc_1,Expenses_Provns_Deprc_2)
remove(Expenses_Provns_Deprc_1,Expenses_Provns_Deprc_2)
Expenses_Provns_Deprc_un <- Expenses_Provns_Deprc_un %>% pivot_longer(.,2:ncol(Expenses_Provns_Deprc_un), names_to = "vars", values_to = "vals") %>% 
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


Expenses_Provns_Deprc_un <- select(Expenses_Provns_Deprc_un,-c(Provns,Deprc))

#Expenses-Amortisation/Write-off

Expenses_Amort_Write_offs_1<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_Amortisation_Write-off_unlist_1.xlsx")
Expenses_Amort_Write_offs_2<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_Amortisation_Write-off_unlist_2.xlsx")
Expenses_Amort_Write_offs_un<-bind_rows(Expenses_Amort_Write_offs_1,Expenses_Amort_Write_offs_2)
remove(Expenses_Amort_Write_offs_1,Expenses_Amort_Write_offs_2)
Expenses_Amort_Write_offs_un <- Expenses_Amort_Write_offs_un %>% pivot_longer(.,2:ncol(Expenses_Amort_Write_offs_un), names_to = "vars", values_to = "vals") %>% 
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


Expenses_Amort_Write_offs_un <- select(Expenses_Amort_Write_offs_un,-c(Amort,Write_offs))     


#Expenses-Auditor-Consultancy fees

Expenses_Auditor_Consultancy_Fees_un<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_Auditor_Consultancy_Fees_unlisted_21yrs.xlsx")

Expenses_Auditor_Consultancy_Fees_un <- Expenses_Auditor_Consultancy_Fees_un %>% pivot_longer(.,2:ncol(Expenses_Auditor_Consultancy_Fees_un), names_to = "vars", values_to = "vals") %>% 
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
Expenses_ITES_Travel_un<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_ITES_Travel_unlisted_21yrs.xlsx")

Expenses_ITES_Travel_un<- Expenses_ITES_Travel_un %>% pivot_longer(.,2:ncol(Expenses_ITES_Travel_un), names_to = "vars", values_to = "vals") %>% 
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

Expenses_RawMaterials_IndirectTaxes_un<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_RawMaterials_IndirectTaxes_unlisted_21yrs.xlsx")

Expenses_RawMaterials_IndirectTaxes_un<- Expenses_RawMaterials_IndirectTaxes_un %>% pivot_longer(.,2:ncol(Expenses_RawMaterials_IndirectTaxes_un), names_to = "vars", values_to = "vals") %>% 
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

Expenses_RawMaterials_IndirectTaxes_un <- select(Expenses_RawMaterials_IndirectTaxes_un,-c(Pkg_expenses))

#Expenses_Miscellaneous
Expenses_Miscellaneous_un<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Expenses_Miscellaneous_unlisted_21_yrs.xlsx")

Expenses_Miscellaneous_un<- Expenses_Miscellaneous_un %>% pivot_longer(.,2:ncol(Expenses_Miscellaneous_un), names_to = "vars", values_to = "vals") %>% 
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

Total_Expenses_un<-left_join(Expenses21_32_un, Expenses11_20_un, by= c("Co_name","year")) %>%
  left_join(.,Expenses1_10_un, by=c("Co_name","year")) %>%
  left_join(.,Expenses_Provns_Deprc_un, by=c("Co_name","year")) %>%
  left_join(.,Expenses_Amort_Write_offs_un, by=c("Co_name","year")) %>%
  left_join(.,Expenses_Auditor_Consultancy_Fees_un, by=c("Co_name","year")) %>%
  left_join(.,Expenses_ITES_Travel_un, by=c("Co_name","year")) %>%
  left_join(.,Expenses_RawMaterials_IndirectTaxes_un, by=c("Co_name","year")) %>%
  left_join(.,Expenses_Miscellaneous_un, by=c("Co_name","year")) %>%
  left_join(.,ChangeinStock_un, by=c("Co_name","year")) %>%
  left_join(.,CntgtLiabilities_un, by=c("Co_name","year")) %>%
  left_join(.,CSR_Disclosures_un, by=c("Co_name","year"))

remove(Expenses1_10_un,Expenses11_20_un,Expenses21_32_un,Expenses_Provns_Deprc_un,Expenses_Amort_Write_offs_un,
       Expenses_Auditor_Consultancy_Fees_un,Expenses_ITES_Travel_un,Expenses_RawMaterials_IndirectTaxes_un,Expenses_Miscellaneous_un,
       ChangeinStock_un,CntgtLiabilities_un,CSR_Disclosures_un)

DerivedIndicatorsOfExpenses_1<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Derived_unlisted_1.xlsx")
DerivedIndicatorsOfExpenses_2<- readxl::read_excel("./Prowess/Unlisted_firms/Formatted/Derived_unlisted_2.xlsx")
DerivedIndicatorsOfExpenses_un<-bind_rows(DerivedIndicatorsOfExpenses_1,DerivedIndicatorsOfExpenses_2)
remove(DerivedIndicatorsOfExpenses_1,DerivedIndicatorsOfExpenses_2)
DerivedIndicatorsOfExpenses_un<- DerivedIndicatorsOfExpenses_un %>% pivot_longer(.,2:ncol(DerivedIndicatorsOfExpenses_un), names_to = "vars", values_to = "vals") %>%
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

Total_Expenses_un<-left_join(DerivedIndicatorsOfExpenses_un, Total_Expenses_un, by= c("Co_name","year"))

remove(DerivedIndicatorsOfExpenses_un)
#eeptools::isid(Total_Expenses_un, vars = c("Co_name", "year"))


# Merging FV_1 and Total_Expenses ---------------------------------------------------

FV_2_un <- left_join(Total_Expenses_un, FV_1_un, by=c("Co_name","year"))

remove(FV_1_un,Total_Expenses_un)

#eeptools::isid(FV_2_un, vars = c("Co_name", "year"))



# Assets unlist Cleaning ---------------------------------------------------------
assets_1<- readxl::read_excel("./Prowess/Unlisted_firms/Assets/Assets_2000_2010.xlsx")
assets_1<-assets_1 %>%pivot_longer(.,2:ncol(assets_1), names_to = "vars", values_to = "vals")%>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>%
  mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Tot_assets=`Total assets`,
         Non_Curr_assets=`Non-current assets`,
         Gppe=`Gross property, plant and equipment`,
         NFA=`Net fixed assets`,
         GIA=`Gross intangible assets`,
         NIA=`Net intangible assets`,
         GFA=`Gross fixed assets`,
         Curr_assets=`Current assets (incl. short term investments, loans & advances)`,
         Cash_bal=`Cash balance`,
         Bank_bal=`Bank balance (short term)`,
         Avg_TA=`Average total assets`,
         Avg_Deb=`Average debtors`,
         Avg_nw=`Average net worth`,
         Avg_cred=`Average creditors`,
         Avg_fg=`Average stock of finished goods`,
         Short_term_trade_and_bills_receivable=`Short term trade receivables & bills receivable`)
assets_2<- readxl::read_excel("./Prowess/Unlisted_firms/Assets/Assets_2011_2020.xlsx")
assets_2<-assets_2 %>%pivot_longer(.,2:ncol(assets_2), names_to = "vars", values_to = "vals")%>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>%
  mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Tot_assets=`Total assets`,
         Non_Curr_assets=`Non-current assets`,
         Gppe=`Gross property, plant and equipment`,
         NFA=`Net fixed assets`,
         GIA=`Gross intangible assets`,
         NIA=`Net intangible assets`,
         GFA=`Gross fixed assets`,
         Curr_assets=`Current assets (incl. short term investments, loans & advances)`,
         Cash_bal=`Cash balance`,
         Bank_bal=`Bank balance (short term)`,
         Avg_TA=`Average total assets`,
         Avg_Deb=`Average debtors`,
         Avg_nw=`Average net worth`,
         Avg_cred=`Average creditors`,
         Avg_fg=`Average stock of finished goods`,
         Short_term_trade_and_bills_receivable=`Short term trade receivables & bills receivable`)


assets_un<-bind_rows(assets_1,assets_2) 
# emp_branches unlist -----------------------------------------------------------------
emp_branches<-readxl::read_excel("./Prowess/Unlisted_firms/emp_branches.xlsx")
emp_branches<-emp_branches %>% pivot_longer(.,2:ncol(emp_branches), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Employees=`No. of employees`,
         Sh_Outside_India=`No.of shareholders outside India`,
         Branches=`No. of branches`)
# Receivables and Inventors unlist------------------------------------------------------------------------
Receivables_Inventory<-readxl::read_excel("./Prowess/Unlisted_firms/Receivables_Inventory.xlsx")
Receivables_Inventory<-Receivables_Inventory %>% pivot_longer(.,2:ncol(Receivables_Inventory), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`)
assets_un<-left_join(assets_un,emp_branches,by=c("Co_name","year")) %>% 
  left_join(.,Receivables_Inventory,by=c("Co_name","year"))
remove(assets_1,assets_2,emp_branches,Receivables_Inventory)
#eeptools::isid(assets_un, vars = c("Co_name", "year"))
# capital Cleaning unlist ----------------------------------------------------

capital_1<-readxl::read_excel("./Prowess/Unlisted_firms/Capital/capital_2000_2010.xlsx")
capital_1<-capital_1 %>%pivot_longer(.,2:ncol(capital_1), names_to = "vars", values_to = "vals")%>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>%
  mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         total_capital=`Total capital`,
         paid_eqcapital=`Paid up equity capital (net of forfeited equity capital)`,
         fulpaid_eqcapital=`Fully paid up equity capital`,
         parpaid_eqcapital=`Partly paid up equity capital (net of forfeited capital)`,
         for_eqcap=`Forfeited equity capital`,
         paid_prefcapital=`Paid up preference capital (net of forfeited preference capital)`,
         fulpaid_prefcapital=`Fully paid up preference capital`,
         parpaid_prefcapital=`Partly paid up preference capital (net of forfeited capital)`,
         cap_contrib_by_govt_oth=`Equity contributions / securities in the nature of capital`,
         eqcont_govt=`Equity contribution from government`,
         moneyrec_ag_conv_sh_war=`Money received against convertible share warrants`,
         hybridorcapitalsecs=`Hybrid perpetual/capital securities`)

capital_2<-readxl::read_excel("./Prowess/Unlisted_firms/Capital/capital_2011_2020.xlsx")
capital_2<-capital_2 %>%pivot_longer(.,2:ncol(capital_2), names_to = "vars", values_to = "vals")%>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>%
  mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         total_capital=`Total capital`,
         paid_eqcapital=`Paid up equity capital (net of forfeited equity capital)`,
         fulpaid_eqcapital=`Fully paid up equity capital`,
         parpaid_eqcapital=`Partly paid up equity capital (net of forfeited capital)`,
         for_eqcap=`Forfeited equity capital`,
         paid_prefcapital=`Paid up preference capital (net of forfeited preference capital)`,
         fulpaid_prefcapital=`Fully paid up preference capital`,
         parpaid_prefcapital=`Partly paid up preference capital (net of forfeited capital)`,
         cap_contrib_by_govt_oth=`Equity contributions / securities in the nature of capital`,
         eqcont_govt=`Equity contribution from government`,
         moneyrec_ag_conv_sh_war=`Money received against convertible share warrants`,
         hybridorcapitalsecs=`Hybrid perpetual/capital securities`)
capital_un<-bind_rows(capital_1,capital_2)
remove(capital_1,capital_2)
#eeptools::isid(capital_un, vars = c("Co_name", "year"))
# working capital unlist ---------------------------------------------------------

workingcapital_1<- readxl::read_excel("./Prowess/Unlisted_firms/Working_capital/workincapital_2000_2010.xlsx")
workingcapital_1<-workingcapital_1 %>%pivot_longer(.,2:ncol(workingcapital_1), names_to = "vars", values_to = "vals")%>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>%
  mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         gwc_cs=`Gross working capital (cost of sales method)`,
         nwc=`Net working capital`,
         quickratio=`Quick ratio (times)`,
         currentratio=`Current ratio (times)`,
         debtequity=`Debt to equity ratio (times)`,
         dscr=`DSCR (times)`,
         cashtocl=`Cash to current liabilities (times)`,
         rmturnover=`Raw material turnover (times)`,
         wipturnover=`WIP turnover (times)`,
         fgturnover=`Finished goods turnover (times)`,
         creditorsturnover  =`Creditors turnover (times)`,
         debtorsturnover=`Debtors turnover (times)`,
         gfa_utl_ratio=`Gross fixed assets utilisation ratio(times)`,
         emp_utl_ratio=`Employees utilisation ratio(times)`,
         nfa_utl_ratio=`Net fixed assets utilisation ratio(times)`)
workingcapital_2<- readxl::read_excel("./Prowess/Unlisted_firms/Working_capital/workincapital_2011_2020.xlsx")
workingcapital_2<-workingcapital_2 %>%pivot_longer(.,2:ncol(workingcapital_2), names_to = "vars", values_to = "vals")%>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>%
  mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         gwc_cs=`Gross working capital (cost of sales method)`,
         nwc=`Net working capital`,
         quickratio=`Quick ratio (times)`,
         currentratio=`Current ratio (times)`,
         debtequity=`Debt to equity ratio (times)`,
         dscr=`DSCR (times)`,
         cashtocl=`Cash to current liabilities (times)`,
         rmturnover=`Raw material turnover (times)`,
         wipturnover=`WIP turnover (times)`,
         fgturnover=`Finished goods turnover (times)`,
         creditorsturnover=`Creditors turnover (times)`,
         debtorsturnover=`Debtors turnover (times)`,
         gfa_utl_ratio=`Gross fixed assets utilisation ratio(times)`,
         emp_utl_ratio=`Employees utilisation ratio(times)`,
         nfa_utl_ratio=`Net fixed assets utilisation ratio(times)`)
workingcapital_un<-bind_rows(workingcapital_1,workingcapital_2)
remove(workingcapital_1,workingcapital_2)
#eeptools::isid(workingcapital_un, vars = c("Co_name", "year"))

# RM Consumed unlist -------------------------------------------------------------
rmconsumed_un<- readxl::read_excel("./Prowess/Unlisted_firms/rmc.xlsx")
rmconsumed_un<-rmconsumed_un %>%pivot_longer(.,2:ncol(rmconsumed_un), names_to = "vars", values_to = "vals")%>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>%
  mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         rmc=`Raw materials consumed`,
         irmc=`Indigenous raw materials consumed`,
         importedrmc=`Imported raw materials consumed`,
         otherrmc=`Other materials consumed`,
         sto_sparesconsumed=`Stores & spares(components) consumed`,
         Exp_earnings=`Export earnings`,
         exporttosales_ratio=`Export / Sales (%)`,
         tforexearnings_ti_ratio=`Total forex earnings / Total income (%)`,
         rmimportstormpurchases_ratio=`Raw material imports / Raw material purchases (%)`)
#eeptools::isid(rmconsumed_un, vars = c("Co_name", "year"))

# liabilities unlist -------------------------------------------------------------
Liab_1_1<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_1_1.xlsx")
Liab_2_1<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/unlisted_2/liab_unlist_2_1.xlsx")
Liab_11<-bind_rows(Liab_1_1,Liab_2_1)
Liab_11<-Liab_11%>% pivot_longer(.,2:ncol(Liab_11), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_liab=`Total liabilities`,
         Reserves_fund=`Reserves and funds`,
         Sec_premium_res=`Security premium reserves (net of deductions)`,
         Capital_res=`Capital, debt, investment & other reserves`,
         Reval_res=`Revaluation reserves`,
         Employee_res=`Employee stock option reserve`,
         General_rese=`General reserves`,
         Surplus_deficit=`Surplus/deficit as at the end of the year`,
         Revenue_exp_charged_res=`Revenue expenses directly charged to reserves`,
         Sh_Appl_mon=`Share application money & suspense account`)
Liab_1_2<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_1_2.xlsx")
Liab_2_2<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/unlisted_2/liab_unlist_2_2.xlsx")
Liab_12<-bind_rows(Liab_1_2,Liab_2_2)
Liab_12<-Liab_12%>% pivot_longer(.,2:ncol(Liab_12), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Sh_Appl_mon_adv_equity=`Share application money and advances -- equity`,
         Sh_Appl_mon_adv_pref=`Share application money and advances -- preference shares`,
         Equity_cap_susp=`Equity capital suspense`,
         Pref_cap_susp=`Preference capital suspense account`,
         Non_curr_liab=`Non-current liabilities`,
         Ltb_excl_curr_portion=`Long term borrowings excl current portion`,
         Ltb_banks=`Long term borrowing from banks`,
         Ltb_FI=`Long term borrowing from financial institutions`,
         Ltb_Cent_State_govt=`Long term borrowings from central & state govt`,
         Ltb_synd_bank_fi=`Long term borrowings syndicated across banks & institutions`)
Liab_1_3<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_1_3.xlsx")
Liab_2_3<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/unlisted_2/liab_unlist_2_3.xlsx")
Liab_13<-bind_rows(Liab_1_3,Liab_2_3)
Liab_13<-Liab_13%>% pivot_longer(.,2:ncol(Liab_13), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Lt_debs_bonds=`Long term debentures and bonds`,
         Sec_Lt_debs_bonds=`Secured long term debentures and bonds`,
         Unsec_Lt_debs_bonds=`Unsecured long term debentures and bonds`,
         Curr_por_lt_debs_bonds=`Current portion of long term debentures and bonds`,
         Lt_foreign_curr_borr=`Long term foreign currency borrowings`,
         Lt_loans_prom_dir_sh=`Long term loans from promoters, directors and shareholders (individuals)`,
         Lt_IC_loans=`Long term inter-corporate loans`,
         Lt_def_credit=`Long term deferred credit`,
         curr_liab=`Current liabilities`,
         St_borr=`Short-term borrowings`)
Liab_1_4<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_1_4.xlsx")
Liab_2_4<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/unlisted_2/liab_unlist_2_4.xlsx")
Liab_14<-bind_rows(Liab_1_4,Liab_2_4)
Liab_14<-Liab_14%>% pivot_longer(.,2:ncol(Liab_14), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         St_trpay_accep=`Short term trade payables and acceptances`,
         curr_mat_ltdebt_lease=`Current maturities of long term debt & lease`,
         Dep_adv_cust_emp_st=`Deposits & advances from customers and employees (short term)`,
         Int_accr_not_due_st=`Interest accrued but not due (short term)`,
         Sh_app_mon_adv_oversubs_refundable=`Share application money and advances - oversubscribed and refundable amount`,
         Other_curr_liab=`Other current liabilities`,
         Prov_outstand_st=`Provisions outstanding (short term)`,
         Corp_tax_prov_st=`Corporate tax provision (short term)`,
         Oth_dir_ind_tax_prov_st=`Other direct & indirect tax provisions (short term)`,
         Prov_bad_doubt_adv_deb_st=`Provision for bad and doubtful advances and debts (short term)`)
Liab_1_11<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_1_11.xlsx")
Liab_2_11<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_2_11.xlsx")
Liab_15<-bind_rows(Liab_1_11,Liab_2_11)
Liab_15<-Liab_15%>% pivot_longer(.,2:ncol(Liab_15), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Dividend_prov=`Dividend provisions`,
         Div_tax_prov=`Dividend tax provision`,
         Prov_emp_benfits_st=`Provision for employee benefits (short term)`,
         St_prov_resto_cost=`Short term provision for restoration costs`,
         Oth_st_prov=`Other short term provisions`,
         Of_whch_curr_liab_prov_ssis_smes=`Of which current liabilities and provisions due to ssis and smes`,
         Sh_funds=`Shareholders funds`,
         Networth=`Net worth`,
         Tan_networth=`Tangible net worth`,
         Sh_app_mon_adv=`Share application money and advances (Eq & Pref)`)
Liab_1_21<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_1_21.xlsx")
Liab_2_21<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_2_21.xlsx")
Liab_16<-bind_rows(Liab_1_21,Liab_2_21)
Liab_16<-Liab_16%>% pivot_longer(.,2:ncol(Liab_16), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         cum_retained_profits=`Cumulative retained profits`,
         Free_res=`Free reserves`,
         Specific_res=`Specific reserves`,
         Total_outside_liab=`Total outside liabilities`,
         Total_term_liab=`Total term liabilities`,
         Curr_liab_incl_long_term_port=`Current liabilities incl long term portion`,
         Cost_production_wip=`Cost of production - work in progress`,
         Dec_work_capital=`Decrease increase in working capital`,
         Capital_empl=`Capital employed`,
         Tol_tnw_times=`TOL/TNW (times)`)
Liab_1_31<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_1_31.xlsx")
Liab_2_31<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_2_31.xlsx")
Liab_17<-bind_rows(Liab_1_31,Liab_2_31)
Liab_17<-Liab_17%>% pivot_longer(.,2:ncol(Liab_17), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_liabilities_wip=`Total term liabilities / tangible net worth`,
         Cont_liab_by_networth=`Contingent liabilities / Net worth (%)`,
         Tot_inte_by_officeadjrecv=`Total inter office adj recv`,
         Tot_leas_by_rentrecv=`Total lease rent recv`,
         Tot_oth_receivables=`Total other receivables`,
         Tot_oth_currentliabilities=`Total other current liabilities`,
         Tot_oth_nonbankingcurrentassets=`Total other non-banking current assets`,
         Tot_receiv_by_duetoforeignexchangefluctuations=`Total recveivables due to foreign exchange fluctuations`,
         Tot_receiv_by_forsaleofinvestments=`Total receivables for sale of investments`,
         Net_def_taxliabilities=`Net deferred tax liabilities`)
Liab_1_41<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_1_41.xlsx")
Liab_2_41<-readxl::read_excel("./Prowess/Unlisted_firms/Formatted/liabilities_unlisted/liab_unlist_2_41.xlsx")
Liab_18<-bind_rows(Liab_1_41,Liab_2_41)
Liab_18<-Liab_18%>% pivot_longer(.,2:ncol(Liab_18), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Net_def_taxliabilitiesasofnetworth=`Net deferred tax liabilities as % of net worth`,
         Net_def_by_taxliabilitiesasoftotalliabilities=`Net deferred tax liabilities as % of total liabilities`,
         Deposits_acc_commer_banks=`Deposits (accepted by commercial banks)`,
         Demand_deposits=`Demand deposits`,
         Demand_deposits_banks=`Demand deposits from banks`,
         Demand_deposits_others=`Demand deposits from others`,
         Saving_deposits=`Saving deposits`,
         Term_deposits=`Term deposits`,
         Term_deposits_banks=`Term deposits from banks`,
         Term_deposits_others=`Term deposits from others`)
liabilities_deposits_un<-left_join(Liab_18,Liab_11,by=c("Co_name","year")) %>% 
  left_join(.,Liab_12,by=c("Co_name","year")) %>% 
  left_join(.,Liab_13,by=c("Co_name","year")) %>%
  left_join(.,Liab_14,by=c("Co_name","year")) %>%
  left_join(.,Liab_15,by=c("Co_name","year")) %>%
  left_join(.,Liab_16,by=c("Co_name","year")) %>%
  left_join(.,Liab_17,by=c("Co_name","year")) 
remove(Liab_1_1,Liab_1_11,Liab_1_2,Liab_1_21,Liab_1_3,Liab_1_31,Liab_1_4,Liab_1_41,
       Liab_11,Liab_12,Liab_13,Liab_14,Liab_15,Liab_16,Liab_17,Liab_18,Liab_2_41,Liab_2_1,
       Liab_2_11,Liab_2_2,Liab_2_21,Liab_2_3,Liab_2_31,Liab_2_4)
#eeptools::isid(liabilities_deposits_un, vars = c("Co_name", "year"))


# Directors Salary unlist --------------------------------------------------------
Dir_salary_un<-readxl::read_excel("./Prowess/Unlisted_firms/Directors Salary.xlsx")

Dir_salary_un<- Dir_salary_un %>% pivot_longer(.,2:(ncol(Dir_salary_un)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% 
  rename(Co_name = `Company Name`,
         Dir_salary=`Directors' salary`,
         Dir_sitting_fee_comm_nonexec_dir=`Director's sitting fees and commission to non-executive director`,
         Dir_bonus_and_commission=`Directors' bonus and commission`,
         Dir_perquisites=`Directors' perquisites`,
         Dir_retirement_benefits=`Directors' retirement benefits`,
         Dir_cont_pf=`Directors' contribution to PF`,
         Exec_dir_remuneration=`Executive directors' remuneration`,
         Kmp_salary=`KMP salary`,
         Kmp_total_remuneration=`KMP total remuneration`)

# cfo unlist ---------------------------------------------------------------------

cfo_1<- readxl::read_excel("./Prowess/Unlisted_firms/CFO/cfo1.xlsx")
cfo_2<- readxl::read_excel("./Prowess/Unlisted_firms/CFO/cfo2.xlsx")
cfo_un<-bind_rows(cfo_1,cfo_2)
cfo_un<-cfo_un %>% pivot_longer(.,2:ncol(cfo_un), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Cfo = `Cash flow from operating activites`,
         Net_CashFlow_OperAct=`Net cash flow from operating activities`,
         Net_CashInOutFlow_InvAct=`Net cash inflow or (outflow) from investing activities`,
         Net_CashInOutFlow_FinAct=`Net cash inflow or (outflow) from financing activities`,
         Cash_in_Yrbegin=`Cash and cash equivalents as at the beginning of the year`,
         Cash_in_Yrend=`Cash and cash equivalents as at the end of the year`,
         Tot_forex_earngs=`Total forex earnings`,
         Export_goods=`Export of goods(fob)`,
         Forex_earngs_interest=`Forex earning -- interest`,
         Forex_earngs_dvnd=`Forex earning -- dividend`,
         Other_forex_earngs=`Other forex earnings`,
         Deemed_rpt=`Deemed export`,
         Export_svcs=`Export of services`)

#eeptools::isid(cfo_un, vars = c("Co_name", "year"))

remove(cfo_1, cfo_2)

# Auditor Name unlist ------------------------------------------------------------

Auditors_1<-readxl::read_excel("./Prowess/Unlisted_firms/Auditors/auditors_2000_2010.xlsx") 
Auditors_2<-readxl::read_excel("./Prowess/Unlisted_firms/Auditors/auditors_2011_2020.xlsx") 

Big4<-readxl::read_excel("./Prowess/Unlisted_firms/Auditors/Big4_list.xlsx") %>% 
  rename(Auditor_name = `Auditor Name`)

Auditors_un<-bind_rows(Auditors_1,Auditors_2)
Auditors_un<-Auditors_un %>% rename(Co_name = `Company Name`,
                                    Auditor_name = `Auditor Name`) %>% select(-2,-3) 
Auditors_un<-left_join(Auditors_un, Big4, by="Auditor_name")
Auditors_un<-Auditors_un %>% mutate(
  big4= case_when(
    is.na(Big4) ~ 0,
    Big4 == "1" ~ 1,
    TRUE ~ 2)) %>% select(-4) %>%rename(Big4=`big4`) 
remove(Auditors_1,Auditors_2,Big4)


# Return Ratios unlist -----------------------------------------------------------

return_ratios_un<-readxl::read_excel("./Prowess/Unlisted_firms/Return_ratios.xlsx")

return_ratios_un<- return_ratios_un %>% pivot_longer(.,2:(ncol(return_ratios_un)), names_to = "vars", values_to = "vals") %>% 
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% 
  pivot_wider(., names_from = "vars", values_from = "vals")%>% rename(Co_name = `Company Name`,
                                                                      Roa=`Return on total assets`,
                                                                      Roce=`Return on capital employed`,
                                                                      Ronw=`Return on net worth`)

# NSE Indicators unlist--------------------------------------------------------------------
Nse_indicators_un<- readxl::read_excel("./Prowess/Unlisted_firms/Unlisted_Identity_Indcators.xlsx") %>% 
  rename(Co_name=`Company Name`,
         Prowess_code=`Prowess company code`,
         Incorp_year=`Incorporation year`,
         Indust_type=`Industry type`,
         Main_Prod_service_group=`Main product/service group`,
         Main_Prod_service_group_code=`Main product/service group code`,
         Nic_code=`NIC code`,
         Ownership_group=`Ownership group`,
         ISIN=`ISIN code`,
         Nse_symbol=`NSE symbol`,
         Bse_scrip_code=`BSE scrip code`,
         Bse_scrip_id=`BSE scrip id`)

# Equity Ownership Unlisted-----------------------------------------------------------------
Equity_1<-readxl::read_excel("./Prowess/Unlisted_firms/Equity Ownership Unlisted/eq_2000_2005.xlsx")
Equity_1<-Equity_1 %>%pivot_longer(.,2:ncol(Equity_1), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_shares_num_shares_held=`Total Shares (In nos) - Shares held`,
         Promoters_in_num_shares_held=`Promoters (In nos) - Shares held`,
         Non_promoters_num_promoters_held=`Non-promoters (In nos) - Shares held`,
         Non_prom_inst_num_shares_held=`Non-promoter Institutions (In nos) - Shares held`,
         Non_prom_mf_uti_num_shares_held=`Non-promoter Mutual  Funds/ UTI (In nos) - Shares held`,
         Non_prom_bfi_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In nos) - Shares held`,
         Non_prom_insu_companies_num_shares_held=`Non-promoter Insurance Companies (In nos) - Shares held`,
         Non_prom_fi_banks_num_shares_held=`Non-promoter Financial Institutions & Banks (In nos) - Shares held`,
         Non_prom_cen_state_num_shares_held=`Non-promoter Central & State Government (In nos) - Shares held`,
         Non_prom_fiis_num_shares_held=`Non-promoter FIIs (In nos) - Shares held`,
         Non_prom_vc_num_shares_held=`Non-promoter Venture  Capital Funds (In nos) - Shares held`,
         Non_prom_foreig_vc_num_shares_held=`Non-promoter Foreign Venture Capital (In nos) - Shares held`,
         Non_prom_qfi_inst_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In nos) - Shares held`,
         Other_Institutional_Non_prom_promoters_num_shares_held=`Other Institutional Non-promoters (In nos) - Shares held`,
         Non_prom_non_institutions_num_shares_held=`Non-promoter Non-institutions (In nos) - Shares held`,
         Non_prom_corp_bodies_num_shares_held=`Non-promoter Corporate Bodies (In nos) - Shares held`,
         Non_prom_indi_num_shares_held=`Non-promoter Individuals (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_num_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In nos) - Shares held`,
         Non_prom_qfi_num_shares_held=`Non-promoter Qualified Foreign Investor (In nos) - Shares held`,
         Oth_non_inst_non_prom_num_shares_held=`Other Non-institutional Non-promoters (In nos) - Shares held`,
         Custodians_num_shares_held=`Shares  held  by Custodians (In nos) - Shares held`,
         Total_shares_percent_shares_held=`Total Shares (In %) - Shares held`,
         Promoters_percent_shares_held=`Promoters (In %) - Shares held`,
         Indian_promoters_percent_shares_held=`Indian Promoters (In %) - Shares held`,
         Indian_promoter_Individuals_HUF_percent_shares_held=`Indian Promoter Individuals & HUF (In %) - Shares held`,
         Indian_cen_state_prom_percent_Sha__num_shares_held =`Indian Central & State Govt. Promoters (In %) - Shares held`,
         Indian_prom_corp_bodies_percent_shares_held=`Indian Promoter Corporate Bodies (In %) - Shares held`,
         Indian_prom_FI_Banks_shares_held =`Indian Promoter FIs & Banks (In %) - Shares held`,
         Other_indian_promoters_percent_shares_held=`Other Indian Promoters (In %) - Shares held`,
         Foreign_prom_percent_shares_held=`Foreign Promoters (In %) - Shares held`,
         Foreign_individuals_prom_per_shares_held=`Foreign Individuals (NRIs) Promoters (In %) - Shares held`,
         Foreign_corp_prom_per_shares_held=`Foreign Promoter Corporate Bodies (In %) - Shares held`,
         Foreign_promoter_institutions_num_promoter_held=`Foreign Promoter Institutions (In %) - Shares held`,
         Non_promoter_fiis_percent_num_shares_held=`Non-promoter FIIs (In %) - Shares held`,
         Non_prom_vc_percent_num_shares_held=`Non-promoter Venture  Capital Funds (In %) - Shares held`,
         Non_prom_foreign_vc_percent_num_shares_held=`Non-promoter Foreign Venture Capital (In %) - Shares held`,
         Non_prom_qfi_percent_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In %) - Shares held`,
         Other_Institutional_Non_prom_inst_num_shares_held=`Other Institutional Non-promoters (In %) - Shares held`,
         Non_prom_non_inst_percent_shares_held=`Non-promoter Non-institutions (In %) - Shares held`,
         Non_prom_corp_percent_shares_held=`Non-promoter Corporate Bodies (In %) - Shares held`,
         Non_promoter_individuals_percent_promoter_held=`Non-promoter Individuals (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_upto_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_over_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In %) - Shares held`,
         Non_prom_qfi_percent_shares_held=`Non-promoter Qualified Foreign Investor (In %) - Shares held`,
         Oth_non_inst_non_prom_percent_shares_held=`Other Non-institutional Non-promoters (In %) - Shares held`,
         Custodians_percent_shares_held=`Shares  held  by Custodians (In %) - Shares held`,
         Indian_promoters_num_shares_held=`Indian Promoters (In nos) - Shares held`,
         Indian_promoter_Individuals_HUF_num_shares_held=`Indian Promoter Individuals & HUF (In nos) - Shares held`,
         Indian_cen_state_prom_num_shares_held=`Indian Central & State Govt. Promoters (In nos) - Shares held`,
         Indian_prom_corp_bodies_num_shares_held=`Indian Promoter Corporate Bodies (In nos) - Shares held`,
         Indian_prom_FI_Banks_num_shares_held =`Indian Promoter FIs & Banks (In nos) - Shares held`,
         Other_indian_promoters_num_indian_held=`Other Indian Promoters (In nos) - Shares held`,
         Foreign_prom_num_shares_held=`Foreign Promoters (In nos) - Shares held`,
         Foreign_individuals_prom__nos_shares_held=`Foreign Individuals (NRIs) Promoters (In nos) - Shares held`,
         Foreign_corp_prom_nos_shares_held=`Foreign Promoter Corporate Bodies (In nos) - Shares held`,
         Foreign_prom_inst_num_shares_held=`Foreign Promoter Institutions (In nos) - Shares held`,
         Prom_qfi_num_shares_held=`Promoter Qualified Foreign Investor (In nos) - Shares held`,
         Other_foreign_promoters_num_foreign_held_nos=`Other Foreign Promoters (In nos) - Shares held`,
         Persons_act_concert_prom_num_shares_held=`Persons acting in concert as promoters (In nos) - Shares held`,
         Non_prom_insu_percent_shares_held=`Non-promoter Insurance Companies (In %) - Shares held`,
         Non_prom_fi_banks_percent_shares_held=`Non-promoter Financial Institutions & Banks (In %) - Shares held`,
         Non_prom_cen_state_percent_shares_held=`Non-promoter Central & State Government (In %) - Shares held`,
         Promoter_Qualified_Foreign_Investor_per_shares_held= `Promoter Qualified Foreign Investor (In %) - Shares held`,
         Other_foreign_promoters_num_foreign_held_per=`Other Foreign Promoters (In %) - Shares held`,
         Persons_act_concert_prom_percent_shares_held=`Persons acting in concert as promoters (In %) - Shares held`,
         Non_promoters_percent_shares_held=`Non-promoters (In %) - Shares held`,
         Non_prom_inst_percent_shares_held=`Non-promoter Institutions (In %) - Shares held`,
         Non_prom_mf_uti_percent_shares_held=`Non-promoter Mutual  Funds/ UTI (In %) - Shares held`,
         Non_prom_bfi_percent_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In %) - Shares held`)
Equity_2<-readxl::read_excel("./Prowess/Unlisted_firms/Equity Ownership Unlisted/eq_2006_2010.xlsx")
Equity_2<-Equity_2 %>%pivot_longer(.,2:ncol(Equity_2), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_shares_num_shares_held=`Total Shares (In nos) - Shares held`,
         Promoters_in_num_shares_held=`Promoters (In nos) - Shares held`,
         Non_promoters_num_promoters_held=`Non-promoters (In nos) - Shares held`,
         Non_prom_inst_num_shares_held=`Non-promoter Institutions (In nos) - Shares held`,
         Non_prom_mf_uti_num_shares_held=`Non-promoter Mutual  Funds/ UTI (In nos) - Shares held`,
         Non_prom_bfi_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In nos) - Shares held`,
         Non_prom_insu_companies_num_shares_held=`Non-promoter Insurance Companies (In nos) - Shares held`,
         Non_prom_fi_banks_num_shares_held=`Non-promoter Financial Institutions & Banks (In nos) - Shares held`,
         Non_prom_cen_state_num_shares_held=`Non-promoter Central & State Government (In nos) - Shares held`,
         Non_prom_fiis_num_shares_held=`Non-promoter FIIs (In nos) - Shares held`,
         Non_prom_vc_num_shares_held=`Non-promoter Venture  Capital Funds (In nos) - Shares held`,
         Non_prom_foreig_vc_num_shares_held=`Non-promoter Foreign Venture Capital (In nos) - Shares held`,
         Non_prom_qfi_inst_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In nos) - Shares held`,
         Other_Institutional_Non_prom_promoters_num_shares_held=`Other Institutional Non-promoters (In nos) - Shares held`,
         Non_prom_non_institutions_num_shares_held=`Non-promoter Non-institutions (In nos) - Shares held`,
         Non_prom_corp_bodies_num_shares_held=`Non-promoter Corporate Bodies (In nos) - Shares held`,
         Non_prom_indi_num_shares_held=`Non-promoter Individuals (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_num_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In nos) - Shares held`,
         Non_prom_qfi_num_shares_held=`Non-promoter Qualified Foreign Investor (In nos) - Shares held`,
         Oth_non_inst_non_prom_num_shares_held=`Other Non-institutional Non-promoters (In nos) - Shares held`,
         Custodians_num_shares_held=`Shares  held  by Custodians (In nos) - Shares held`,
         Total_shares_percent_shares_held=`Total Shares (In %) - Shares held`,
         Promoters_percent_shares_held=`Promoters (In %) - Shares held`,
         Indian_promoters_percent_shares_held=`Indian Promoters (In %) - Shares held`,
         Indian_promoter_Individuals_HUF_percent_shares_held=`Indian Promoter Individuals & HUF (In %) - Shares held`,
         Indian_cen_state_prom_percent_Sha__num_shares_held =`Indian Central & State Govt. Promoters (In %) - Shares held`,
         Indian_prom_corp_bodies_percent_shares_held=`Indian Promoter Corporate Bodies (In %) - Shares held`,
         Indian_prom_FI_Banks_shares_held =`Indian Promoter FIs & Banks (In %) - Shares held`,
         Other_indian_promoters_percent_shares_held=`Other Indian Promoters (In %) - Shares held`,
         Foreign_prom_percent_shares_held=`Foreign Promoters (In %) - Shares held`,
         Foreign_individuals_prom_per_shares_held=`Foreign Individuals (NRIs) Promoters (In %) - Shares held`,
         Foreign_corp_prom_per_shares_held=`Foreign Promoter Corporate Bodies (In %) - Shares held`,
         Foreign_promoter_institutions_num_promoter_held=`Foreign Promoter Institutions (In %) - Shares held`,
         Non_promoter_fiis_percent_num_shares_held=`Non-promoter FIIs (In %) - Shares held`,
         Non_prom_vc_percent_num_shares_held=`Non-promoter Venture  Capital Funds (In %) - Shares held`,
         Non_prom_foreign_vc_percent_num_shares_held=`Non-promoter Foreign Venture Capital (In %) - Shares held`,
         Non_prom_qfi_percent_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In %) - Shares held`,
         Other_Institutional_Non_prom_inst_num_shares_held=`Other Institutional Non-promoters (In %) - Shares held`,
         Non_prom_non_inst_percent_shares_held=`Non-promoter Non-institutions (In %) - Shares held`,
         Non_prom_corp_percent_shares_held=`Non-promoter Corporate Bodies (In %) - Shares held`,
         Non_promoter_individuals_percent_promoter_held=`Non-promoter Individuals (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_upto_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_over_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In %) - Shares held`,
         Non_prom_qfi_percent_shares_held=`Non-promoter Qualified Foreign Investor (In %) - Shares held`,
         Oth_non_inst_non_prom_percent_shares_held=`Other Non-institutional Non-promoters (In %) - Shares held`,
         Custodians_percent_shares_held=`Shares  held  by Custodians (In %) - Shares held`,
         Indian_promoters_num_shares_held=`Indian Promoters (In nos) - Shares held`,
         Indian_promoter_Individuals_HUF_num_shares_held=`Indian Promoter Individuals & HUF (In nos) - Shares held`,
         Indian_cen_state_prom_num_shares_held=`Indian Central & State Govt. Promoters (In nos) - Shares held`,
         Indian_prom_corp_bodies_num_shares_held=`Indian Promoter Corporate Bodies (In nos) - Shares held`,
         Indian_prom_FI_Banks_num_shares_held =`Indian Promoter FIs & Banks (In nos) - Shares held`,
         Other_indian_promoters_num_indian_held=`Other Indian Promoters (In nos) - Shares held`,
         Foreign_prom_num_shares_held=`Foreign Promoters (In nos) - Shares held`,
         Foreign_individuals_prom__nos_shares_held=`Foreign Individuals (NRIs) Promoters (In nos) - Shares held`,
         Foreign_corp_prom_nos_shares_held=`Foreign Promoter Corporate Bodies (In nos) - Shares held`,
         Foreign_prom_inst_num_shares_held=`Foreign Promoter Institutions (In nos) - Shares held`,
         Prom_qfi_num_shares_held=`Promoter Qualified Foreign Investor (In nos) - Shares held`,
         Other_foreign_promoters_num_foreign_held_nos=`Other Foreign Promoters (In nos) - Shares held`,
         Persons_act_concert_prom_num_shares_held=`Persons acting in concert as promoters (In nos) - Shares held`,
         Non_prom_insu_percent_shares_held=`Non-promoter Insurance Companies (In %) - Shares held`,
         Non_prom_fi_banks_percent_shares_held=`Non-promoter Financial Institutions & Banks (In %) - Shares held`,
         Non_prom_cen_state_percent_shares_held=`Non-promoter Central & State Government (In %) - Shares held`,
         Promoter_Qualified_Foreign_Investor_per_shares_held= `Promoter Qualified Foreign Investor (In %) - Shares held`,
         Other_foreign_promoters_num_foreign_held_per=`Other Foreign Promoters (In %) - Shares held`,
         Persons_act_concert_prom_percent_shares_held=`Persons acting in concert as promoters (In %) - Shares held`,
         Non_promoters_percent_shares_held=`Non-promoters (In %) - Shares held`,
         Non_prom_inst_percent_shares_held=`Non-promoter Institutions (In %) - Shares held`,
         Non_prom_mf_uti_percent_shares_held=`Non-promoter Mutual  Funds/ UTI (In %) - Shares held`,
         Non_prom_bfi_percent_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In %) - Shares held`)
Equity_3<-readxl::read_excel("./Prowess/Unlisted_firms/Equity Ownership Unlisted/eq_2011_2015.xlsx")
Equity_3<-Equity_3 %>%pivot_longer(.,2:ncol(Equity_3), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_shares_num_shares_held=`Total Shares (In nos) - Shares held`,
         Promoters_in_num_shares_held=`Promoters (In nos) - Shares held`,
         Non_promoters_num_promoters_held=`Non-promoters (In nos) - Shares held`,
         Non_prom_inst_num_shares_held=`Non-promoter Institutions (In nos) - Shares held`,
         Non_prom_mf_uti_num_shares_held=`Non-promoter Mutual  Funds/ UTI (In nos) - Shares held`,
         Non_prom_bfi_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In nos) - Shares held`,
         Non_prom_insu_companies_num_shares_held=`Non-promoter Insurance Companies (In nos) - Shares held`,
         Non_prom_fi_banks_num_shares_held=`Non-promoter Financial Institutions & Banks (In nos) - Shares held`,
         Non_prom_cen_state_num_shares_held=`Non-promoter Central & State Government (In nos) - Shares held`,
         Non_prom_fiis_num_shares_held=`Non-promoter FIIs (In nos) - Shares held`,
         Non_prom_vc_num_shares_held=`Non-promoter Venture  Capital Funds (In nos) - Shares held`,
         Non_prom_foreig_vc_num_shares_held=`Non-promoter Foreign Venture Capital (In nos) - Shares held`,
         Non_prom_qfi_inst_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In nos) - Shares held`,
         Other_Institutional_Non_prom_promoters_num_shares_held=`Other Institutional Non-promoters (In nos) - Shares held`,
         Non_prom_non_institutions_num_shares_held=`Non-promoter Non-institutions (In nos) - Shares held`,
         Non_prom_corp_bodies_num_shares_held=`Non-promoter Corporate Bodies (In nos) - Shares held`,
         Non_prom_indi_num_shares_held=`Non-promoter Individuals (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_num_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In nos) - Shares held`,
         Non_prom_qfi_num_shares_held=`Non-promoter Qualified Foreign Investor (In nos) - Shares held`,
         Oth_non_inst_non_prom_num_shares_held=`Other Non-institutional Non-promoters (In nos) - Shares held`,
         Custodians_num_shares_held=`Shares  held  by Custodians (In nos) - Shares held`,
         Total_shares_percent_shares_held=`Total Shares (In %) - Shares held`,
         Promoters_percent_shares_held=`Promoters (In %) - Shares held`,
         Indian_promoters_percent_shares_held=`Indian Promoters (In %) - Shares held`,
         Indian_promoter_Individuals_HUF_percent_shares_held=`Indian Promoter Individuals & HUF (In %) - Shares held`,
         Indian_cen_state_prom_percent_Sha__num_shares_held =`Indian Central & State Govt. Promoters (In %) - Shares held`,
         Indian_prom_corp_bodies_percent_shares_held=`Indian Promoter Corporate Bodies (In %) - Shares held`,
         Indian_prom_FI_Banks_shares_held =`Indian Promoter FIs & Banks (In %) - Shares held`,
         Other_indian_promoters_percent_shares_held=`Other Indian Promoters (In %) - Shares held`,
         Foreign_prom_percent_shares_held=`Foreign Promoters (In %) - Shares held`,
         Foreign_individuals_prom_per_shares_held=`Foreign Individuals (NRIs) Promoters (In %) - Shares held`,
         Foreign_corp_prom_per_shares_held=`Foreign Promoter Corporate Bodies (In %) - Shares held`,
         Foreign_promoter_institutions_num_promoter_held=`Foreign Promoter Institutions (In %) - Shares held`,
         Non_promoter_fiis_percent_num_shares_held=`Non-promoter FIIs (In %) - Shares held`,
         Non_prom_vc_percent_num_shares_held=`Non-promoter Venture  Capital Funds (In %) - Shares held`,
         Non_prom_foreign_vc_percent_num_shares_held=`Non-promoter Foreign Venture Capital (In %) - Shares held`,
         Non_prom_qfi_percent_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In %) - Shares held`,
         Other_Institutional_Non_prom_inst_num_shares_held=`Other Institutional Non-promoters (In %) - Shares held`,
         Non_prom_non_inst_percent_shares_held=`Non-promoter Non-institutions (In %) - Shares held`,
         Non_prom_corp_percent_shares_held=`Non-promoter Corporate Bodies (In %) - Shares held`,
         Non_promoter_individuals_percent_promoter_held=`Non-promoter Individuals (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_upto_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_over_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In %) - Shares held`,
         Non_prom_qfi_percent_shares_held=`Non-promoter Qualified Foreign Investor (In %) - Shares held`,
         Oth_non_inst_non_prom_percent_shares_held=`Other Non-institutional Non-promoters (In %) - Shares held`,
         Custodians_percent_shares_held=`Shares  held  by Custodians (In %) - Shares held`,
         Indian_promoters_num_shares_held=`Indian Promoters (In nos) - Shares held`,
         Indian_promoter_Individuals_HUF_num_shares_held=`Indian Promoter Individuals & HUF (In nos) - Shares held`,
         Indian_cen_state_prom_num_shares_held=`Indian Central & State Govt. Promoters (In nos) - Shares held`,
         Indian_prom_corp_bodies_num_shares_held=`Indian Promoter Corporate Bodies (In nos) - Shares held`,
         Indian_prom_FI_Banks_num_shares_held =`Indian Promoter FIs & Banks (In nos) - Shares held`,
         Other_indian_promoters_num_indian_held=`Other Indian Promoters (In nos) - Shares held`,
         Foreign_prom_num_shares_held=`Foreign Promoters (In nos) - Shares held`,
         Foreign_individuals_prom__nos_shares_held=`Foreign Individuals (NRIs) Promoters (In nos) - Shares held`,
         Foreign_corp_prom_nos_shares_held=`Foreign Promoter Corporate Bodies (In nos) - Shares held`,
         Foreign_prom_inst_num_shares_held=`Foreign Promoter Institutions (In nos) - Shares held`,
         Prom_qfi_num_shares_held=`Promoter Qualified Foreign Investor (In nos) - Shares held`,
         Other_foreign_promoters_num_foreign_held_nos=`Other Foreign Promoters (In nos) - Shares held`,
         Persons_act_concert_prom_num_shares_held=`Persons acting in concert as promoters (In nos) - Shares held`,
         Non_prom_insu_percent_shares_held=`Non-promoter Insurance Companies (In %) - Shares held`,
         Non_prom_fi_banks_percent_shares_held=`Non-promoter Financial Institutions & Banks (In %) - Shares held`,
         Non_prom_cen_state_percent_shares_held=`Non-promoter Central & State Government (In %) - Shares held`,
         Promoter_Qualified_Foreign_Investor_per_shares_held= `Promoter Qualified Foreign Investor (In %) - Shares held`,
         Other_foreign_promoters_num_foreign_held_per=`Other Foreign Promoters (In %) - Shares held`,
         Persons_act_concert_prom_percent_shares_held=`Persons acting in concert as promoters (In %) - Shares held`,
         Non_promoters_percent_shares_held=`Non-promoters (In %) - Shares held`,
         Non_prom_inst_percent_shares_held=`Non-promoter Institutions (In %) - Shares held`,
         Non_prom_mf_uti_percent_shares_held=`Non-promoter Mutual  Funds/ UTI (In %) - Shares held`,
         Non_prom_bfi_percent_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In %) - Shares held`)
Equity_4<-readxl::read_excel("./Prowess/Unlisted_firms/Equity Ownership Unlisted/eq_2016_2020.xlsx")
Equity_4<-Equity_4 %>%pivot_longer(.,2:ncol(Equity_4), names_to = "vars", values_to = "vals") %>%
  mutate(year=as.double(str_sub(vars, start=-4L))) %>% mutate(vars=str_sub(vars, end=-5L)) %>% pivot_wider(., names_from = "vars", values_from = "vals") %>% 
  rename(Co_name = `Company Name`,
         Total_shares_num_shares_held=`Total Shares (In nos) - Shares held`,
         Promoters_in_num_shares_held=`Promoters (In nos) - Shares held`,
         Non_promoters_num_promoters_held=`Non-promoters (In nos) - Shares held`,
         Non_prom_inst_num_shares_held=`Non-promoter Institutions (In nos) - Shares held`,
         Non_prom_mf_uti_num_shares_held=`Non-promoter Mutual  Funds/ UTI (In nos) - Shares held`,
         Non_prom_bfi_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In nos) - Shares held`,
         Non_prom_insu_companies_num_shares_held=`Non-promoter Insurance Companies (In nos) - Shares held`,
         Non_prom_fi_banks_num_shares_held=`Non-promoter Financial Institutions & Banks (In nos) - Shares held`,
         Non_prom_cen_state_num_shares_held=`Non-promoter Central & State Government (In nos) - Shares held`,
         Non_prom_fiis_num_shares_held=`Non-promoter FIIs (In nos) - Shares held`,
         Non_prom_vc_num_shares_held=`Non-promoter Venture  Capital Funds (In nos) - Shares held`,
         Non_prom_foreig_vc_num_shares_held=`Non-promoter Foreign Venture Capital (In nos) - Shares held`,
         Non_prom_qfi_inst_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In nos) - Shares held`,
         Other_Institutional_Non_prom_promoters_num_shares_held=`Other Institutional Non-promoters (In nos) - Shares held`,
         Non_prom_non_institutions_num_shares_held=`Non-promoter Non-institutions (In nos) - Shares held`,
         Non_prom_corp_bodies_num_shares_held=`Non-promoter Corporate Bodies (In nos) - Shares held`,
         Non_prom_indi_num_shares_held=`Non-promoter Individuals (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In nos) - Shares held`,
         Non_prom_hol_nominal_invst_num_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In nos) - Shares held`,
         Non_prom_qfi_num_shares_held=`Non-promoter Qualified Foreign Investor (In nos) - Shares held`,
         Oth_non_inst_non_prom_num_shares_held=`Other Non-institutional Non-promoters (In nos) - Shares held`,
         Custodians_num_shares_held=`Shares  held  by Custodians (In nos) - Shares held`,
         Total_shares_percent_shares_held=`Total Shares (In %) - Shares held`,
         Promoters_percent_shares_held=`Promoters (In %) - Shares held`,
         Indian_promoters_percent_shares_held=`Indian Promoters (In %) - Shares held`,
         Indian_promoter_Individuals_HUF_percent_shares_held=`Indian Promoter Individuals & HUF (In %) - Shares held`,
         Indian_cen_state_prom_percent_Sha__num_shares_held =`Indian Central & State Govt. Promoters (In %) - Shares held`,
         Indian_prom_corp_bodies_percent_shares_held=`Indian Promoter Corporate Bodies (In %) - Shares held`,
         Indian_prom_FI_Banks_shares_held =`Indian Promoter FIs & Banks (In %) - Shares held`,
         Other_indian_promoters_percent_shares_held=`Other Indian Promoters (In %) - Shares held`,
         Foreign_prom_percent_shares_held=`Foreign Promoters (In %) - Shares held`,
         Foreign_individuals_prom_per_shares_held=`Foreign Individuals (NRIs) Promoters (In %) - Shares held`,
         Foreign_corp_prom_per_shares_held=`Foreign Promoter Corporate Bodies (In %) - Shares held`,
         Foreign_promoter_institutions_num_promoter_held=`Foreign Promoter Institutions (In %) - Shares held`,
         Non_promoter_fiis_percent_num_shares_held=`Non-promoter FIIs (In %) - Shares held`,
         Non_prom_vc_percent_num_shares_held=`Non-promoter Venture  Capital Funds (In %) - Shares held`,
         Non_prom_foreign_vc_percent_num_shares_held=`Non-promoter Foreign Venture Capital (In %) - Shares held`,
         Non_prom_qfi_percent_num_shares_held=`Non-promoter Qualified Foreign Investor - Institutions (In %) - Shares held`,
         Other_Institutional_Non_prom_inst_num_shares_held=`Other Institutional Non-promoters (In %) - Shares held`,
         Non_prom_non_inst_percent_shares_held=`Non-promoter Non-institutions (In %) - Shares held`,
         Non_prom_corp_percent_shares_held=`Non-promoter Corporate Bodies (In %) - Shares held`,
         Non_promoter_individuals_percent_promoter_held=`Non-promoter Individuals (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_upto_shares_held=`Non-promoter Investors holding nominal invest. upto Rs 1 lakh (In %) - Shares held`,
         Non_prom_hol_nominal_invst_percent_over_shares_held=`Non-promoter Investors holding nominal invest. over Rs 1 lakh (In %) - Shares held`,
         Non_prom_qfi_percent_shares_held=`Non-promoter Qualified Foreign Investor (In %) - Shares held`,
         Oth_non_inst_non_prom_percent_shares_held=`Other Non-institutional Non-promoters (In %) - Shares held`,
         Custodians_percent_shares_held=`Shares  held  by Custodians (In %) - Shares held`,
         Indian_promoters_num_shares_held=`Indian Promoters (In nos) - Shares held`,
         Indian_promoter_Individuals_HUF_num_shares_held=`Indian Promoter Individuals & HUF (In nos) - Shares held`,
         Indian_cen_state_prom_num_shares_held=`Indian Central & State Govt. Promoters (In nos) - Shares held`,
         Indian_prom_corp_bodies_num_shares_held=`Indian Promoter Corporate Bodies (In nos) - Shares held`,
         Indian_prom_FI_Banks_num_shares_held =`Indian Promoter FIs & Banks (In nos) - Shares held`,
         Other_indian_promoters_num_indian_held=`Other Indian Promoters (In nos) - Shares held`,
         Foreign_prom_num_shares_held=`Foreign Promoters (In nos) - Shares held`,
         Foreign_individuals_prom__nos_shares_held=`Foreign Individuals (NRIs) Promoters (In nos) - Shares held`,
         Foreign_corp_prom_nos_shares_held=`Foreign Promoter Corporate Bodies (In nos) - Shares held`,
         Foreign_prom_inst_num_shares_held=`Foreign Promoter Institutions (In nos) - Shares held`,
         Prom_qfi_num_shares_held=`Promoter Qualified Foreign Investor (In nos) - Shares held`,
         Other_foreign_promoters_num_foreign_held_nos=`Other Foreign Promoters (In nos) - Shares held`,
         Persons_act_concert_prom_num_shares_held=`Persons acting in concert as promoters (In nos) - Shares held`,
         Non_prom_insu_percent_shares_held=`Non-promoter Insurance Companies (In %) - Shares held`,
         Non_prom_fi_banks_percent_shares_held=`Non-promoter Financial Institutions & Banks (In %) - Shares held`,
         Non_prom_cen_state_percent_shares_held=`Non-promoter Central & State Government (In %) - Shares held`,
         Promoter_Qualified_Foreign_Investor_per_shares_held= `Promoter Qualified Foreign Investor (In %) - Shares held`,
         Other_foreign_promoters_num_foreign_held_per=`Other Foreign Promoters (In %) - Shares held`,
         Persons_act_concert_prom_percent_shares_held=`Persons acting in concert as promoters (In %) - Shares held`,
         Non_promoters_percent_shares_held=`Non-promoters (In %) - Shares held`,
         Non_prom_inst_percent_shares_held=`Non-promoter Institutions (In %) - Shares held`,
         Non_prom_mf_uti_percent_shares_held=`Non-promoter Mutual  Funds/ UTI (In %) - Shares held`,
         Non_prom_bfi_percent_num_shares_held=`Non-promoter Banks, FI's, Insurance Cos. (In %) - Shares held`)
Equity_un<-bind_rows(Equity_1,Equity_2,Equity_3,Equity_4)
remove(Equity_1,Equity_2,Equity_3,Equity_4)
#eeptools::isid(Equity_un, vars = c("Co_name", "year"))

# Creating FV_un final --------------------------------------------------------


FV_un<-left_join(FV_2_un, assets_un, by=c("Co_name","year")) %>%
  left_join(., liabilities_deposits_un, by= c("Co_name","year")) %>%
  left_join(., capital_un, by= c("Co_name","year")) %>% 
  left_join(., workingcapital_un, by= c("Co_name","year")) %>% 
  left_join(., rmconsumed_un, by= c("Co_name","year")) %>%
  left_join(., cfo_un, by= c("Co_name","year")) %>% 
  left_join(., return_ratios_un, by= c("Co_name","year")) %>% 
  left_join(., Equity_un,by=c("Co_name","year"))%>% 
  left_join(., Dir_salary_un,by=c("Co_name","year")) %>% 
  left_join(., Nse_indicators_un, by= c("Co_name")) %>% 
  mutate(unlisted=1)

#eeptools::isid(FV_un, vars = c("Co_name", "year"))

Aud_NSE_un<-left_join(Auditors_un, Nse_indicators_un, by= c("Co_name"))                         

Aud_NSE_un<-Aud_NSE_un %>% filter(Indust_type!=3) #dups verified and then filtered to remove banks. wait until confirmation to merge with FV

remove(FV_2_un, assets_un, liabilities_deposits_un, capital_un, workingcapital_un,rmconsumed_un, cfo_un,
       return_ratios_un, Dir_salary_un, Equity_un)











# Merging FV_un to Auditors --------------------------------------------------

 
 FV_un<-left_join(FV_un, Auditors_un)

eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))






remove(Auditors, Auditors_un, NSE_indicators, Nse_indicators_un)




# Appending FV Listed and Unlisted ----------------------------------------

FV<-bind_rows(FV, FV_un)

remove(FV_un)

FV<-FV %>% mutate(Groupstand = case_when(
  is.na(Groupstand) & unlisted==1 ~ 1,
  is.na(Groupstand) ~ 0,
  TRUE ~ Groupstand
))


eeptools::isid(FV, vars = c("Co_name", "year", "Auditor_name"))



# Saving Image ------------------------------------------------------------
save.image("./Prowess/R Data Prowess/FV List-Unlist.Rdata")

