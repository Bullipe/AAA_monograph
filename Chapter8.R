library(tidyverse)
library(GGally)
library(binsreg)

source("accruals.R") #creates file acc_computed.RDS
df_acc1 <- readRDS("acc_computed.RDS")
source("Auditor_tenure.R") #creates file audtenure.RDS
df_tenure <- readRDS("audtenure.RDS")

df_acc <- df_acc1 %>% 
  left_join(df_tenure,by=c("gvkey", "fyear")) %>% 
  filter(if_all(c("res_oa_opacc", "tenure", "age"), ~ !is.na(.x)) )

# Fig 8.2 -----------------------------------------------------------------


df_acc %>% 
  ggpairs(columns=c("tenure","age","res_oa_opacc"),
          lower = list(continuous = wrap("points", position=position_jitter(height=1, width=1),
                                         alpha = 0.1,    size=0.1, color="blue"), 
                       combo = wrap("dot", alpha = 0.4,            size=0.2) ),
          title="Pairwise scatterplots",
          columnLabels = c("tenure","age","Residual optg accr")
  ) 

#ggsave("Pair_plots.svg", device="svg")


# Fig 8.1 -----------------------------------------------------------------

attach(df_acc) #to enable writing "tenure" instead of "df_acc$tenure", etc. 
p_ctrl <- binsreg(y=res_oa_opacc, x= tenure, w=age, #age as single control variable
                  nbins=10, ci=c(0,0), cb=c(0,0)) #10 bins, confidence intervals and 
                                                  #confidence band without spline or smoothing
          #see binsreg package documentation
p_raw <- binsreg(y=res_oa_opacc, x= tenure, #no control variables
                 nbins=10, ci=c(0,0), cb=c(0,0)) #same no. of bins as above for comparison
detach(df_acc)

p_ctrl$bins_plot + 
  labs(y="Residual Operating Accruals",
       x = "Auditor tenure",
  title="Binned scatter plot",
  subtitle = "Panel A: controlling for firm age") 
#ggsave("Binned_control_regression.svg", device="svg")  

p_raw$bins_plot + 
  labs(y="Residual Operating Accruals",
       x = "Auditor tenure",
       title="Binned scatter plot",
       subtitle = "Panel B: without control variables") 
#ggsave("Binned_simple_regression.svg", device="svg")
