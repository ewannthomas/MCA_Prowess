

# Aud_reappoint -----------------------------------------------------------



blah<-FV %>% group_by(Ownership_group, Co_name) %>% 
  mutate(Aud_reappoint = case_when(
    Joint_Auditor==0 & lead(Aud_rotation)==1 ~ Auditor_name,
    Joint_Auditor==0 & Aud_rotation==1 ~ Auditor_name)) %>% 
  select(Ownership_group, Co_name, year, Auditor_name, Aud_rotation, Aud_reappoint) %>%
  group_by(Ownership_group) %>% 
  nest()



flah<-function(x){
  
  y<-intersect(x %>% filter(Aud_rotation==0) %>% pull(Aud_reappoint) %>% unique(), 
               x %>% filter(Aud_rotation==1) %>% pull(Aud_reappoint) %>% unique()) 
}

blah<-blah %>% mutate(vectors= map(data,flah))

blah<-blah %>% unnest(data) %>% ungroup()

blah<-blah %>% mutate(boom= map(data,flah))



blah<-blah %>% mutate(boom=case_when(
  Aud_rotation==0 & Aud_reappoint %in% vectors[[row_number()]]==TRUE ~ 1,
  is.na(Aud_reappoint) ~ NA_real_))


dups=map2_dbl(data, flah, case_when(
  Aud_rotation==1 & Aud_reappoint==any(vectors) ~ 1,
  TRUE ~ NA_real_
)))

id<-blah %>% select(id) %>% pull()

sapply(id, function (x)
  blah<-blah %>% mutate(boom=999,
                        boom=case_when(
                          Aud_rotation==0 & Aud_reappoint %in% vectors[[x]]==TRUE ~ 1,
                          is.na(Aud_reappoint) ~ NA_real_,
                          TRUE ~ boom)))







