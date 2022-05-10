library(tidyverse)
library(modelr) #for add_residual
library(DescTools) #for Winsorize

# Need a data file "data_for_accruals.RDS" containing the relevant variables 
# from Compustat, beginning 1986

##Calculation of accruals defined in Larson et al. RAST, 2018
df_accruals <- readRDS("../Data/data_for_accruals.RDS") %>% 
  filter( if_all(c("at","ceq","emp","ni", "lt","oancf","ivncf"), ~ !is.na(.x)), #all these variables must be nonmissing
          !str_detect(sic,"6\\d\\d\\d")) %>%   #SIC is not 6xxx (financials)
  mutate(citotal = coalesce(citotal,ni), #set citotal = ni if citotal is missing
         across(act:xidoc, ~replace_na(.x, 0)) #for all variables other than the above, set them to zero if missing.
  )  %>% 
  group_by(gvkey) %>% 
  mutate(
    across(c("ceq","che","at","ivaeq","ivao","lt","dlc","dltt","act","lct"), 
           ~ .x - lag(.x), .names = "ch_{.col}"), #create ch_ceq, ch_che etc.
    avg_at = (at + lag(at))/2,
    earn = citotal - dvp +stkco, #comprehensive earnings
    compacc = ch_ceq - ch_che, #comprehensive accruals
    cf = earn - compacc, #comprehensive cash flow
    opacc = (ch_at-ch_che-ch_ivaeq-ch_ivao) - 
      (ch_lt - ch_dlc - ch_dltt), #operating accruals
    wcacc = (ch_at-ch_che) - (ch_lct - ch_dlc), #working capital accruals
    ltacc = opacc - wcacc, #long term accruals
    cc_acc = min((-fopo + txbco +stkco),0) +
      min((xido-xidoc),0), #conditionally conservative articulating accruals
    na_acc = opacc - (ni-dvp+stkco-oancf - 
                        (ivncf+ivch-siv-ivstch)), #non-articulating accruals
    oa_opacc = opacc - cc_acc - na_acc, #other articulating operating accruals
    oa_wcacc = -recch-invch-apalch-txach-aoloch, #other articulating working capital accruals
    oa_ltacc = oa_opacc - oa_wcacc, #other articulating long-term accruals
    finacc = compacc - opacc, #financial accruals
    noa = (at - che - ivaeq - ivao) - (lt - dlc - dltt), #net operating assets
    nca = ceq - che, #net comprehensive assets
    noa_wc = (act - che) - (lct - dlc), #net working capital assets
    noa_lt = noa - noa_wc, #net long-term operating assets
    nfa = nca - noa, #net financial assets
    empgr = if_else(lag(emp)>0.001, emp/lag(emp) -1, NA_real_), #employee growth
    across(c("earn","compacc","cf","opacc","wcacc","ltacc","cc_acc",
             "na_acc","oa_opacc","oa_wcacc","oa_ltacc","finacc","noa","nca",
             "noa_wc","noa_lt","nfa"), ~ .x/avg_at) #deflate by average total assets
  ) %>% 
  select(gvkey, fyear, sic, fyr, compacc, opacc, wcacc, ltacc,
         cc_acc, na_acc, oa_opacc, oa_wcacc, oa_ltacc, finacc,
         noa, nca, noa_wc, noa_lt, nfa, empgr, earn, cf, avg_at) %>% 
  distinct(gvkey,fyear, .keep_all = TRUE) %>%  
  filter(!is.na(avg_at), avg_at>1, !is.na(empgr)) %>% 
  ungroup()
##

##winsorize
df_accruals <- df_accruals %>% 
  mutate(across(compacc:cf, ~Winsorize(.x,probs=c(0.01,0.99),na.rm=TRUE)))   
##

##add residual oa_opacc (using Eq. 13 on p 840 of Larson etal.) to the dataset
m1 <- lm(oa_opacc ~ empgr + empgr:noa, data=df_accruals)
df_accruals <- df_accruals %>% 
  add_residuals(m1,var="res_oa_opacc")   
summary(m1)  

saveRDS(df_accruals,"acc_computed.RDS")