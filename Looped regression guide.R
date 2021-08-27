library(tidyverse)
library(stargazer)

flah<-c("Audit_fees_at", "Non_Audit_Fee_at")
blah<-c("spec_spec", "spec_nonspec", "nonspec_spec", "nonspec_nonspec")
glah<-c("Roy_prod_resid", "Roy_cfo_resid", "Roy_disc_resid", "MJ_roa_resid")
indeps<- paste(c("LOSS", "currentratio", "Invrec_at", "debtequity",
                 "EQFIN", "lag(PB)", "lag(Size)", "lag(Roa)", "merger_acqui_dum",
                 "Promoters_percent_shares_held", "Age", "as.factor(NIC_2)", 
                 "as.factor(year)"), collapse = " + ")


model_df <- expand.grid(y=flah, x=blah, z=glah, indeps = indeps) %>% 
  mutate(form = paste(y, " ~ ", x, "*", z, " + ", indeps)) %>% 
  mutate(output = map(form, ~ FV %>% filter(Groupstand==1, year>2010 & year<2019) %>% 
                        lm(.x, data = .)))



stargazer(model_df$output, out = "test.html", df=F, digits = 1L,
          keep = c("spec_spec", "spec_nonspec", "nonspec_spec", "nonspec_nonspec", 
                   "Roy_prod_resid", "Roy_cfo_resid", "Roy_disc_resid", "MJ_roa_resid"),
          add.lines = list(c("Industry FE", rep(c("YES"), times=32)),
                           c("Year FE", rep(c("YES"), times=32)),
                           c("Controls", rep(c("YES"), times=32))),
          notes = c("Standard Errors in parenthesis"),
          notes.align = "l") 
