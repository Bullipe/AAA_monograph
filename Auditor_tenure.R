library(tidyverse)
library(modelr)

# Pre-requisite: A file Comp.RDS in the current folder, containing the relevant variables from Compustat
df_fs <- readRDS("Comp.RDS") %>% 
  distinct(gvkey,fyear,au, .keep_all = TRUE) %>% #remove duplicates  
  mutate(au=as.integer(au), #map the pre-merger Big-N auditors with the post-merger ones
         merged_au = case_when(au==2 ~ 4L, au==3 ~ 7L, au==8 ~ 5L, au==9 ~ NA_integer_, TRUE ~ au)) %>% 
  filter(!is.na(sale), !is.na(at), !is.na(oiadp), at>1, sale>1) %>% 
  group_by(gvkey) %>%   
  mutate(firstfyr = min(fyear),
         age = fyear - firstfyr + 1,
         aud_episode = case_when(row_number() == 1 ~ 1L,
                                 merged_au != lag(merged_au) ~ row_number(),
                                 TRUE ~ NA_integer_)) %>% #give a new value for aud_episode
  #when there is an auditor change
  fill(aud_episode) #fill the lagged value for aud_episode when there is no change

df_aud1st <- df_fs %>% 
  group_by(gvkey,aud_episode) %>% 
  summarize(firstayr = min(fyear)) #firstayr = first year of each aud_episode to calculate tenure

df_tenure <- df_fs %>% 
  filter(!is.na(merged_au)) %>% 
  left_join(df_aud1st, by=c("gvkey","aud_episode")) %>% 
  mutate(tenure = fyear - firstayr + 1) %>% 
  select(gvkey,fyear,merged_au,firstayr,aud_episode, tenure, age) %>% 
  mutate(B4 = if_else(merged_au < 9,1L,0L)) %>% 
  filter(!is.na(tenure))

m1 <- lm(tenure ~ age, data=df_tenure)
df_tenure <- df_tenure %>% 
  add_residuals(m1,var="tenure_age") 

saveRDS(df_tenure,"audtenure.RDS")