
library(tidyverse)
library(plotly)
library(sjPlot)

load("./Prowess/R Data Prowess/FV Old.Rdata")



# Plotiing top 11 BG reppointment count -----------------------------------


Reappoint_count<-FV %>% group_by(Ownership_group, year) %>% filter(Aud_reappoint==1) %>% 
  distinct(Auditor_name) %>% count() %>% filter(Ownership_group %in% c("Tata Group","Reliance Group [Mukesh Ambani]","I.T.C. (F) Group",
                                                                      "Vedanta Group", "Birla Aditya Group","Uni Lever (F) Group",
                                                                      "Bharti Telecom Group","WIPRO Group","Larsen & Toubro Group",
                                                                      "Mahindra & Mahindra Group","Sun Pharmaceutical Group"))

Reappoint_count<-FV %>%  select(Ownership_group, year, group_GRPT) %>%
  group_by(Ownership_group, year) %>% distinct() %>% 
  left_join(Reappoint_count, .)


plot<-Reappoint_count %>% 
  ggplot(aes(year))+
  geom_line(aes(y=log(group_GRPT)))+
  geom_line(aes(y=n, color=factor(Ownership_group)))+
  facet_wrap(~Ownership_group)+
  geom_vline(xintercept = 2014, color="red",linetype="dashed")+ 
  geom_vline(xintercept = 2018, color="red",linetype="dashed")+
  ylab("Number of Reappointed Auditors & Log of Group RPTs")+
  ggtitle("Trend of Number of Reappointed Auditors")+
  labs(caption="Black line - per year group RPT")+
  theme(legend.position = "none")

ggplotly(plot) 

rm(Reappoint_count, plot)
  

# Post and Pre law reappointment count table ------------------------------


  FV<- FV %>% mutate(Time_dummy_new = case_when(
    year<=2014 ~ 0,
    year>2014 ~ 1,
    TRUE ~ NA_real_))
  
  Pre_post_law_reapp_count<-FV %>% group_by(Ownership_group, Time_dummy_new) %>% 
    filter(Aud_reappoint==1) %>% 
    distinct(Auditor_name) %>% count() %>% 
    pivot_wider(names_from = "Time_dummy_new", values_from = "n") %>% 
    rename(Before_2014 = `0`,
           After_2014 = `1`) %>% 
    mutate(Total_reappointments = sum(c(Before_2014, After_2014, na.rm=TRUE)))
  
 # save(Pre_post_law_reapp_count, file = "./Prowess/R Data Prowess/Pre and Post Law Reappointment count.Rdata")
  
  
  

# Year trend-reappointment ----------------------------------

  
  reapp_per_year<-FV %>% group_by(year, Aud_reappoint) %>% 
    distinct(Auditor_name) %>% count() %>% na.exclude()
  
   plot<-reapp_per_year %>% ggplot(aes(year, y=n))+
    geom_col(aes(y=n, fill=Aud_reappoint, alpha=0.1))+
    geom_vline(xintercept = 2014, color="red",linetype="dash")+
    geom_vline(xintercept = 2018, color="red",linetype="dash")+
    ylab("Number of Reappointed Auditors")+
    ggtitle("Trend of Number of Reappointed Auditors")+
     theme_minimal()
    
    ggplotly(plot)

    

# Year trend-reappointment in number of companies -----------


    
reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1) %>% 
  group_by(year) %>% distinct(Co_name) %>% count()


Unlist_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1) %>% 
  group_by(year, unlisted) %>% distinct(Co_name) %>% count() %>% 
  mutate(unlisted=as.character(unlisted)) %>% 
  rename(unlist_count=n)

BG_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1) %>% 
  group_by(year, Groupstand) %>% distinct(Co_name) %>% count() %>% 
  mutate(Groupstand=as.character(Groupstand)) %>% 
  rename(bg_count=n)


plot<-ggplot(data=reapp_co_count, aes(year))+
  geom_line(aes(y=n))+
  geom_vline(xintercept = 2014, color="red",linetype="dash")+
  geom_vline(xintercept = 2018, color="red",linetype="dash")+
  ylab("Number of Companies with reappointed auditors")+
  ggtitle("Trend of companies with reappointed auditors")+
  theme_minimal()
ggplotly(plot)


ggplot(data=Unlist_count, aes(x=year, y=unlist_count))+
  geom_line(aes(fill=unlisted))+
  geom_point(aes(shape=unlisted))+
  geom_line(data=BG_count, aes(y=bg_count, color=Groupstand))+
  geom_vline(xintercept = 2014, color="red", linetype="dotdash")+
  geom_vline(xintercept = 2018, color="red", linetype="dotdash")+
  theme(legend.position = "bottom")+
  ylab("Number of Companies with reappointed auditors")+
  ggtitle("Trend of companies with reappointed auditors")+
  theme_minimal()
ggplotly(plot)




    
    

# Year trend-reappointment in BG number of companies + RPT --------


reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year) %>% distinct(Co_name) %>% count()


reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year    ) %>% 
  mutate(year_RPT = sum(GRPT, na.rm = T)) %>% 
  distinct(year_RPT) %>% 
  left_join(reapp_co_count, .)


plot<-reapp_co_count %>% 
  ggplot(aes(x=year, y=log(n)))+
  geom_line(color="green")+
  geom_line(aes(y=log(year_RPT)), color="red")+
  geom_point(aes(y=log(year_RPT)), color="red")+
  geom_vline(xintercept = 2014, color="red",linetype="dashed")+ 
  geom_vline(xintercept = 2018, color="red",linetype="dashed")+
  annotate("text", x=2020, y=18.69226,label="Log of year RPT")+
  annotate("text", x=2018, y=7.56008,label="Log of number of group companies")+
  annotate("text", x=2015, y=19.56008,label="Negative RPTs in 2015")+
  ylab("Log of number of group companies with reappointed auditors & Log of Group RPTs")+
  ggtitle("Trend of group companies with reappointed auditors")

ggplotly(plot)


#using plotly
plot_ly(reapp_co_count) %>% 
  add_trace(x=~year,y=~n,type="bar",name="BG Company reappointment count") %>% 
  add_trace(x=~year,y=~year_RPT, type="scatter", mode="line",yaxis="y2",name="Yearly RPTs") %>% 
  layout(yaxis2=list(overlaying="y",side="right")) %>% 
  add_segments(x=2014, xend = 2014, y=0, yend = 4500, color=I("red"), name="year - 2014", 
               line = list(dash="dash")) %>% 
  add_segments(x=2018, xend = 2018, y=0, yend = 4500, color=I("maroon"), name="year - 2018",
               line = list(dash="dash"))

rm(reapp_co_count)
    



# Year trend-reappointment in BG number of companies + Promoter Holding --------

reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year) %>% distinct(Co_name) %>% count()


reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year) %>% 
  mutate(year_prom = mean(Promoters_percent_shares_held, na.rm = T)/100) %>% 
  distinct(year_prom) %>% 
  left_join(reapp_co_count, .)


plot_ly(reapp_co_count) %>% 
  add_trace(x=~year,y=~n,type="bar",name="BG Company reappointment count") %>% 
  add_trace(x=~year,y=~year_prom, type="scatter", mode="line",yaxis="y2",name="Average Promoters % shares held") %>% 
  layout(yaxis2=list(overlaying="y",side="right", title="% shares held", tickformat="%", titlefont=list(size=15)),
         yaxis=list(side="left", title="Number of group companies", titlefont=list(size=15)),
         title="BG company count and average promoter share holding trend") %>% 
  add_segments(x=2014, xend = 2014, y=0, yend = 4500, color=I("red"), name="year - 2014", 
               line = list(dash="dash")) %>% 
  add_segments(x=2018, xend = 2018, y=0, yend = 4500, color=I("maroon"), name="year - 2018",
               line = list(dash="dash"))


rm(reapp_co_count)




# Year trend-reappointment in BG number of companies + Std REM ------------

load("./Prowess/R Data Prowess/Prowess List-Unlist Data.Rdata")

reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year) %>% distinct(Co_name) %>% count()


reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year) %>% 
  mutate(year_std_rem_roy = sum(Std_rem_roy, na.rm = T)) %>% 
  distinct(year_std_rem_roy) %>% 
  left_join(reapp_co_count, .)



plot_ly(reapp_co_count) %>% 
  add_trace(x=~year,y=~n,type="bar",name="BG Company reappointment count") %>% 
  
  add_trace(x=~year,y=~year_std_rem_roy, type="scatter", mode="line",yaxis="y2",name="Aggregate standardized Roy REM") %>% 
  
  layout(yaxis2=list(overlaying="y",side="right", title="Aggregate standardized Roy REM", titlefont=list(size=15)),
         yaxis=list(side="left", title="Number of group companies", titlefont=list(size=15)),
         title="BG company count and Aggregate standardized Roy REM trend",
         annotations=list(x=2025, y=-10,text="Created using reduced sample with 25,512 observations", showarrow=F,
                          xanchor='right', yanchor='auto', xshift=0, yshift=0,
                          font=list(size=10, color="violet"))) %>%
  
  add_segments(x=2014, xend = 2014, y=0, yend = 600, color=I("red"), name="year - 2014", 
               line = list(dash="dash")) %>% 
  
  add_segments(x=2018, xend = 2018, y=0, yend = 600, color=I("maroon"), name="year - 2018",
               line = list(dash="dash"))


rm(reapp_co_count)

# Year trend-reappointment in BG number of companies + Std REM roy2 ------------


reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year) %>% distinct(Co_name) %>% count()


reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year) %>% 
  mutate(year_std_rem_roy2 = sum(Std_rem_roy2, na.rm = T)) %>% 
  distinct(year_std_rem_roy2) %>% 
  left_join(reapp_co_count, .)



plot_ly(reapp_co_count) %>% 
  add_trace(x=~year,y=~n,type="bar",name="BG Company reappointment count") %>% 
  
  add_trace(x=~year,y=~year_std_rem_roy2, type="scatter", mode="line",yaxis="y2",name="Aggregate standardized Roy REM 2") %>% 
  
  layout(yaxis2=list(overlaying="y",side="right", title="Aggregate standardized Roy REM 2", titlefont=list(size=15)),
         yaxis=list(side="left", title="Number of group companies", titlefont=list(size=15)),
         title="BG company count and Aggregate standardized Roy REM trend",
         annotations=list(x=2025, y=-10,text="Created using reduced sample with 25,512 observations", showarrow=F,
                          xanchor='right', yanchor='auto', xshift=0, yshift=0,
                          font=list(size=10, color="violet"))) %>%
  
  add_segments(x=2014, xend = 2014, y=0, yend = 600, color=I("red"), name="year - 2014", 
               line = list(dash="dash")) %>% 
  
  add_segments(x=2018, xend = 2018, y=0, yend = 600, color=I("maroon"), name="year - 2018",
               line = list(dash="dash"))


rm(reapp_co_count)


# Year trend-reappointment in BG number of companies + REM's and MJ separate ------------

load("./Prowess/R Data Prowess/Prowess List-Unlist Data.Rdata")

reapp_co_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year) %>% distinct(Co_name) %>% count()


blah<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year) %>% 
  mutate(year_roy_prdn = sum(Roy_prod_resid , na.rm = T),
         year_roy_cfo = sum(Roy_cfo_resid , na.rm = T),
         year_roy_disc = sum(Roy_disc_resid, na.rm = T),
         year_mj_roa=sum(MJ_roa_resid, na.rm = T))

reapp_co_count<-blah %>% 
  distinct(year_roy_prdn) %>% 
  left_join(reapp_co_count, .)

reapp_co_count<-blah %>% 
  distinct(year_roy_cfo) %>% 
  left_join(reapp_co_count, .)

reapp_co_count<-blah %>% 
  distinct(year_roy_disc) %>% 
  left_join(reapp_co_count, .)

reapp_co_count<-blah %>% 
  distinct(year_mj_roa) %>% 
  left_join(reapp_co_count, .)

plot_ly(reapp_co_count) %>% 
  add_trace(x=~year,y=~n,type="bar",name="BG Company reappointment count", alpha=0.5) %>% 
  
  add_trace(x=~year,y=~year_roy_prdn, type="scatter", mode="line",yaxis="y2",name="Aggregate Roy Production Residual") %>%
  
  add_trace(x=~year,y=~year_roy_cfo, type="scatter", mode="line",yaxis="y2",name="Aggregate Roy CFO Residual") %>%
  
  add_trace(x=~year,y=~year_roy_disc, type="scatter", mode="line",yaxis="y2",name="Aggregate Roy Dsicretionary Residual") %>%
  
  add_trace(x=~year,y=~year_mj_roa, type="scatter", mode="line",yaxis="y2",name="Aggregate MJ RoA Residual") %>%
  
  
  layout(yaxis2=list(overlaying="y",side="right", title="Aggregate Roy & MJ Residuals", titlefont=list(size=15)),
         yaxis=list(side="left", title="Number of group companies", titlefont=list(size=15)),
         title="BG company count and Aggregate Roy & MJ Residuals trend",
         annotations=list(x=2025, y=-10,text="Created using reduced sample with 25,512 observations", showarrow=F,
                          xanchor='right', yanchor='auto', xshift=0, yshift=0,
                          font=list(size=10, color="violet"))) %>%
  
  add_segments(x=2014, xend = 2014, y=0, yend = 600, color=I("red"), name="year - 2014", 
               line = list(dash="dash")) %>% 
  
  add_segments(x=2018, xend = 2018, y=0, yend = 600, color=I("maroon"), name="year - 2018",
               line = list(dash="dash"))


rm(reapp_co_count, blah)


# GAUDSPEC cross tables and plot ---------------------------------------------------

blah<-FV %>% group_by(Ownership_group, GAUDSPEC) %>% distinct(Auditor_name, Aud_reappoint)

tab_xtab(var.col = blah$GAUDSPEC,var.row = blah$Aud_reappoint, show.row.prc = T)

blah<-FV %>% filter(Groupstand==1) %>% group_by(Ownership_group, GAUDSPEC) %>% distinct(Auditor_name, Aud_reappoint)

tab_xtab(var.col = blah$GAUDSPEC,var.row = blah$Aud_reappoint, show.row.prc = T)



reapp_aud_count<-FV %>% group_by(Co_name) %>%
  filter(Aud_reappoint==1 & Groupstand==1) %>% 
  group_by(year,GAUDSPEC) %>% distinct(Auditor_name) %>% count()

reapp_aud_count<-reapp_aud_count %>% mutate(GAUDSPEC=as.character(GAUDSPEC)) %>% 
  pivot_wider(names_from = "GAUDSPEC", values_from = "n")

plot_ly(reapp_aud_count) %>% 
  add_trace(x=~year,y=~`0`,type="bar",name="Below median specialization") %>% 
  
  add_trace(x=~year,y=~`1`, type="bar", name="Above median specialization") %>%  
  
  layout(yaxis=list(side="left", title="Number of Auditors", titlefont=list(size=15)),
         title="BG auditor count as per group auditor specialization") %>% 

  add_segments(x=2014, xend = 2014, y=0, yend = 600, color=I("red"), name="year - 2014", 
               line = list(dash="dash")) %>% 
  
  add_segments(x=2018, xend = 2018, y=0, yend = 600, color=I("maroon"), name="year - 2018",
               line = list(dash="dash"))

rm(blah, reapp_aud_count)
