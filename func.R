library(tidyverse)
library(modelr)
library(lazyeval)


macro_aov <- function (df,x, groupvar, nmin=5) {
# df = dataframe
# x = variable whose within- and between variance is to be analyzed
# groupvar = grouping variable for within
# nmin = minimum number of observations required in each by group

##first winsorize at 0.01 and 0.99
  df1 <- {{df}} %>% 
    select({{x}},{{groupvar}}) %>% 
    filter(!is.na({{x}})) %>% 
    mutate(if_else({{x}} < quantile({{x}},0.01), quantile({{x}},0.01), {{x}}),
            if_else({{x}} > quantile({{x}},0.99), quantile({{x}},0.99), {{x}} )
            )
##
  
  df_group <-  df1 %>%
    group_by({{groupvar}}) %>%
    summarise(group_mean = mean({{x}}),
              group_sd = sd({{x}}),
              n = n()) %>%
    filter(n>= nmin)

  df1 <- df1 %>%
    inner_join(df_group) 
  
  df1 <- df1 %>%
    ungroup()
    
  nobs=nrow(df1)
  nfirms=nrow(df_group)

  attach(df1)
  Total_SS <- sum(({{x}} - mean({{x}}))^2)
  Error_SS <- sum(((df_group$n - 1) * df_group$group_sd^2))
  Group_SS <- Total_SS - Error_SS
  
  Total_df <- length({{x}}) - 1
  Error_df <- length({{x}}) - length(df_group$n)
  Group_df <- Total_df - Error_df
  
  MS_Error <- Error_SS/Error_df
  MS_Group <- Group_SS/Group_df
  ratio = MS_Group/MS_Error
  output = pairlist(MS_Error=MS_Error, MS_Group=MS_Group, Ratio_bet_within=ratio, 
               n_obs=nobs, n_firms=nfirms)
  detach(df1)
  return (output)
}