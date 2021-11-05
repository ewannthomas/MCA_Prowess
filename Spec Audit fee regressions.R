
library(tidyverse)
library(modelsummary)
library(broom)
library(gt)


# Spec_Spec regressions with time filter w/o interactions, with aud_tenure---------------------------------------------------

load("./Prowess/R Data Prowess/FV Old.Rdata")

indep_vars<-FV %>% filter(Groupstand==1, year>2010 & year<2019) %>% select(spec_spec, spec_nonspec, nonspec_spec, nonspec_nonspec)
dep_vars<-FV %>% filter(Groupstand==1,  year>2010 & year<2019) %>% select(Audit_fees_at, Non_Audit_Fee_at)

Aud_reapp_spec<-lapply(dep_vars, function(y)
  lapply(indep_vars, function(x)
    FV %>% filter(Groupstand==1,  year>2010 & year<2019) %>% lm(y ~ x + Aud_tenure +  LOSS + currentratio + Invrec_at + debtequity +
                                                                  EQFIN + lag(PB) + lag(Size) + lag(Roa) + merger_acqui_dum +
                                                                  Promoters_percent_shares_held + Age + as.factor(NIC_2) + as.factor(year), data = ., na.action = na.exclude)))


#renaming iterated indeps

indeps<-list(c("Constant", "x", "Auditor Tenure", "LOSS", "Current Ratio", "Invrec AT",
               "Debt Equity", "EQFIN", "Lag of PB", "Lag of Size", "Lag of RoA",
               "Merger and Acquisition Dummy", "Percent of Shares held by Promters", 
               "Age"))

#removing as.factor coeffs
for(y in 1:2){
for(x in 1:4){
  Aud_reapp_spec[[y]][[x]][[1]]<-Aud_reapp_spec[[y]][[x]][[1]][!(names(Aud_reapp_spec[[y]][[x]][[1]]) %in% 
                                                               c(str_extract(names(Aud_reapp_spec[[y]][[x]][[1]]), 
                                                                             pattern = "[:alpha:]*\\.(.*)")))]
}
}


x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}

aud_names<-c("Specialized - Specialized", "Specialized - Non specialized", "Non specialized - Specialized", "Non specialized - Non specialized")

indeps<-map2(aud_names, indeps, x_replacer)


for(j in 1:2){
  for(i in 1:4){
    Aud_reapp_spec[[j]][[i]]$coefficients<-set_names(Aud_reapp_spec[[j]][[i]]$coefficients, nm=indeps[[i]])
  }
}


for(j in 1:2){
  Aud_reapp_spec[[j]]<-set_names(Aud_reapp_spec[[j]], nm=rep(names(Aud_reapp_spec[j]),times=4))
}


rows<-tibble(col_a<-c("Industry Fixed Effects", "Year Fixed Effects"), 
             col_b<-c("YES"), 
             col_c<-c("YES"),
             col_d<-c("YES"),
             col_e<-c("YES"),
             col_f<-c("YES"),
             col_g<-c("YES"),
             col_h<-c("YES"),
             col_f<-c("YES"))

msummary(c(Aud_reapp_spec[[1]], Aud_reapp_spec[[2]]),
         stars = T,
         add_rows = rows,
         statistic = "statistic", 
         gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
         notes = "t statistic in parenthesis",
         title = "Audit Fee Regression With Time Filter (Audit tenure included)",
output = "Audit Fee Regression With Time Filter (Audit tenure included).html")


rm(indep_vars, dep_vars, x_replacer, aud_names, indeps, j, i, Aud_reapp_spec, x, y, rows)






# Spec_Spec regressions with time filter w/o interactions &  aud_tenure--------

indep_vars<-FV %>% filter(Groupstand==1, year>2010 & year<2019) %>% select(spec_spec, spec_nonspec, nonspec_spec, nonspec_nonspec)
dep_vars<-FV %>% filter(Groupstand==1,  year>2010 & year<2019) %>% select(Audit_fees_at, Non_Audit_Fee_at)

Aud_reapp_spec<-lapply(dep_vars, function(y)
  lapply(indep_vars, function(x)
    FV %>% filter(Groupstand==1,  year>2010 & year<2019) %>% lm(y ~ x + LOSS + currentratio + Invrec_at + debtequity +
                                                                  EQFIN + lag(PB) + lag(Size) + lag(Roa) + merger_acqui_dum +
                                                                  Promoters_percent_shares_held + Age + as.factor(NIC_2) + as.factor(year), data = ., na.action = na.exclude)))


#renaming iterated indeps

indeps<-list(c("Constant", "x", "LOSS", "Current Ratio", "Invrec AT",
               "Debt Equity", "EQFIN", "Lag of PB", "Lag of Size", "Lag of RoA",
               "Merger and Acquisition Dummy", "Percent of Shares held by Promters", 
               "Age"))

#removing as.factor coeffs
for(y in 1:2){
  for(x in 1:4){
    Aud_reapp_spec[[y]][[x]][[1]]<-Aud_reapp_spec[[y]][[x]][[1]][!(names(Aud_reapp_spec[[y]][[x]][[1]]) %in% 
                                                                     c(str_extract(names(Aud_reapp_spec[[y]][[x]][[1]]), 
                                                                                   pattern = "[:alpha:]*\\.(.*)")))]
  }
}


x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}

aud_names<-c("Specialized - Specialized", "Specialized - Non specialized", "Non specialized - Specialized", "Non specialized - Non specialized")

indeps<-map2(aud_names, indeps, x_replacer)


for(j in 1:2){
  for(i in 1:4){
    Aud_reapp_spec[[j]][[i]]$coefficients<-set_names(Aud_reapp_spec[[j]][[i]]$coefficients, nm=indeps[[i]])
  }
}


for(j in 1:2){
  Aud_reapp_spec[[j]]<-set_names(Aud_reapp_spec[[j]], nm=rep(names(Aud_reapp_spec[j]),times=4))
}


rows<-tibble(col_a<-c("Industry Fixed Effects", "Year Fixed Effects"), 
             col_b<-c("YES"), 
             col_c<-c("YES"),
             col_d<-c("YES"),
             col_e<-c("YES"),
             col_f<-c("YES"),
             col_g<-c("YES"),
             col_h<-c("YES"),
             col_f<-c("YES"))


msummary(c(Aud_reapp_spec[[1]], Aud_reapp_spec[[2]]),
         stars = T,
         add_rows = rows,
         statistic = "statistic", 
         gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
         notes = "t statistic in parenthesis",
         title = "Audit Fee Regression With Time Filter ( Without Audit tenure )",
         output = "Audit Fee Regression With Time Filter ( Without Audit tenure ).html")


rm(indep_vars, dep_vars, x_replacer, aud_names, indeps, j, i, Aud_reapp_spec, x, y, rows)



# Spec_Spec regressions with time filter and REM interactions -------------


load("./Prowess/R Data Prowess/Prowess List-Unlist Data.Rdata")

indep_vars<-FV %>% filter(Groupstand==1, year>2010 & year<2019) %>% select(spec_spec, spec_nonspec, nonspec_spec, nonspec_nonspec)
dep_vars<-FV %>% filter(Groupstand==1,  year>2010 & year<2019) %>% select(Audit_fees_at, Non_Audit_Fee_at)
klah<-FV %>% filter(Groupstand==1,  year>2010 & year<2019) %>% select(Roy_prod_resid, Roy_cfo_resid, Roy_disc_resid, MJ_roa_resid) 



Aud_reapp_spec<-lapply(dep_vars, function(y)
  lapply(indep_vars, function(x)
    lapply(klah, function(zz)
    FV %>% filter(Groupstand==1,  year>2010 & year<2019) %>% lm(y ~ x*zz + LOSS + currentratio + Invrec_at + debtequity +
                                                                  EQFIN + lag(PB) + lag(Size) + lag(Roa) + merger_acqui_dum +
                                                                  Promoters_percent_shares_held + Age + as.factor(NIC_2) + as.factor(year), data = ., na.action = na.exclude))))


#renaming iterated indeps

indeps<-list(c("Constant", "x", "zz", 
               "LOSS", "Current Ratio", "Invrec AT",
               "Debt Equity", "EQFIN", "Lag of PB", "Lag of Size", "Lag of RoA",
               "Merger and Acquisition Dummy", "Percent of Shares held by Promters", "Age",
               "x X zz"))


#removing as.factor coeffs
for(z in 1:2){
for(y in 1:4){
  for(x in 1:4){
    Aud_reapp_spec[[z]][[y]][[x]][[1]]<-Aud_reapp_spec[[z]][[y]][[x]][[1]][!(names(Aud_reapp_spec[[z]][[y]][[x]][[1]]) %in% 
                                                                     c(str_extract(names(Aud_reapp_spec[[z]][[y]][[x]][[1]]), 
                                                                                   pattern = "[:alpha:]*\\.(.*)")))]
  }
}
}


x_replacer<-function(h,j){
  str_replace(j, pattern = "x", replacement = h)
}

zz_replacer<-function(h,j){
  str_replace(j, pattern = "zz", replacement = h)
}


aud_names<-c("Specialized - Specialized", "Specialized - Non specialized", "Non specialized - Specialized", "Non specialized - Non specialized")

REM_replacer<-rep(c("Roy Production REM", "Roy CFO REM", "Roy Discretionary REM", "MJ RoA Accruals"), times=4)

indeps<-map2(aud_names, indeps, x_replacer)
indeps<-map2(REM_replacer, indeps, zz_replacer)


for(k in 1:2){
for(j in 1:4){
  for(i in 1:4){
    Aud_reapp_spec[[k]][[j]][[i]]$coefficients<-set_names(Aud_reapp_spec[[k]][[j]][[i]]$coefficients, nm=indeps[[i]])
  }
}
}
  

for(j in 1:2){
  for(k in 1:4){
  Aud_reapp_spec[[j]][[k]]<-set_names(Aud_reapp_spec[[j]][[k]], nm=rep(names(Aud_reapp_spec[j]),times=4))
  }
  }

for(j in 1:2){
    Aud_reapp_spec[[j]]<-set_names(Aud_reapp_spec[[j]], nm=rep(names(Aud_reapp_spec[j]),times=4))
  }






msummary(c(Aud_reapp_spec[[1]][[1]], Aud_reapp_spec[[1]][[2]],Aud_reapp_spec[[1]][[3]], Aud_reapp_spec[[1]][[4]],
           Aud_reapp_spec[[2]][[1]], Aud_reapp_spec[[2]][[2]],Aud_reapp_spec[[2]][[3]], Aud_reapp_spec[[2]][[4]]),
         stars = T,
         statistic = "statistic", 
         gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
         notes = "t statistic in parenthesis",
         title = "Audit Fee Regression With Time Filter with REM Interactions")
         output = "Audit Fee Regression With Time Filter with REM Interactions.html")


rm(indep_vars, dep_vars, x_replacer, aud_names, indeps, j, i, Aud_reapp_spec, x, y, rows)





