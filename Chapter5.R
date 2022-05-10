library(tidyverse)
library(modelr)
library(lazyeval)



# Tenure/Age --------------------------------------------------------------
# Fig 5.1

# Pre-requisite: A file Comp.RDS in the current folder, containing the relevant variables from Compustat
df_fs <- readRDS("Comp.RDS") %>% 
  distinct(gvkey,fyear,au, .keep_all = TRUE) %>% #remove duplicates  
  filter(!is.na(sale), !is.na(at), !is.na(oiadp), at>1, sale>1) %>% 
  group_by(gvkey) %>%   
  mutate(roa = oiadp/at,
         ato = sale/at,
         margin = oiadp/sale,
         leverage = (dltt+dlc)/at) 

source("accruals.R") #This program creates file acc_computed.RDS. 
                    #See accruals.R for input requirements and accruals calculations
df_acc1 <- readRDS("acc_computed.RDS") 

source("Auditor_tenure.R") #This program creates file audtenure.RDS.
df_tenure <- readRDS("audtenure.RDS")

df_acc <- df_acc1 %>% 
  left_join(df_tenure,by=c("gvkey", "fyear")) %>% 
  filter(if_all(c("res_oa_opacc", "tenure", "age"), ~ !is.na(.x)) )

df_acc %>% 
  ggplot() + 
  geom_freqpoly(aes(x=tenure),  binwidth = 1) +
  geom_freqpoly(aes(x=tenure_age), binwidth = 2, color="#00998a") + #histograms
  annotate(geom="text", x=20, y=25000, label="Tenure orthogonalized \nwrt firm age", color="#00998a") +
  annotate(geom="text", x=18, y=5000, label="Tenure", color="black") +
  xlab("Years") +
  ylab("Number of observations") 

#ggsave("tenure.svg", device="svg") #uncomment the beginning of the line 
                                    #to save the plot as svg file

#calculations for within to between ratios reported in 
#"Estimability" section on p. 40 of monograph
source("func.R") #function macro_aov is defined there
aov_roa = macro_aov(df_fs,roa,gvkey)
aov_roa 
aov_margin = macro_aov(df_fs,margin,gvkey)
aov_margin
aov_ato = macro_aov(df_fs,ato,gvkey)
aov_ato
aov_leverage = macro_aov(df_fs,leverage,gvkey)
aov_leverage
aov_tenure = macro_aov(df_tenure,tenure,gvkey)
aov_tenure
aov_tenure_age = macro_aov(df_tenure,tenure_age,gvkey)
aov_tenure_age


# Wage gender height ------------------------------------------------------
#Regression reported in "Nonlinearities" section, Fig 5.2

df_wage <- read.csv("NLSY97data.csv") %>% 
  filter(wages>0, !is.na(male), !is.na(ht_in))

wage_model <- lm(wages ~ male + ht_in + male*ht_in, data=df_wage)
summary(wage_model)
cov_matrix <- vcov(wage_model)        
cov_matrix

coeff <- wage_model$coefficients
x1 <- seq(57,75,0.1)
y1 <- coeff[2] + coeff[4]*x1
se <- sqrt(cov_matrix[2,2] + x1*x1*cov_matrix[4,4] + 2*x1*cov_matrix[2,4])

y_upper <-  y1 + 2*se
y_lower <- y1-2*se

ggplot() +
  geom_line(mapping=aes(y=y1,x=x1),color="blue") + 
  geom_ribbon(aes(ymin=y_lower, ymax=y_upper, x=x1), alpha = 0.3) +
  xlab("Height in inches") +
  ylab("Wage difference in $, male - female") +
  ggtitle("Male-female wage difference by height", subtitle = "Point estimate and 95% confidence interval")

#ggsave("wages.svg", device="svg") #to save the plot in an svg file if desired



# Bayes factor - Fig 5.3 --------------------------------------------------

mu1 = 300
mu2=290
n=25
sd = 30
se= sd/sqrt(n)

#graph x limits
x_low = 278
x_high = 312

ggplot(data.frame(x = c(x_low,x_high)), aes(x = x)) +
  stat_function(fun = dnorm, args=list(mean=mu2,sd=se)) +
  stat_function(fun = dnorm, args=list(mean=mu1,sd=se)) +
  geom_segment(aes(x=293,y=0,xend=293, yend=0.058677)) +
  geom_hline(yintercept=0) +
  xlab("milk powder quantity (g)") +
  ylab("Probability density of sample mean under H1 and H2") +
  annotate(geom="text", x=293, y=-0.002, label="293", color="black") +
  annotate(geom="text", x=284.5, y=0.06, label="H2: mu=290", color="black") +
  annotate(geom="text", x=293.6, y=0.0015, label="A", color="black") +
  annotate(geom="text", x=293.6, y=0.034, label="B", color="black") +
  annotate(geom="text", x=293.6, y=0.059, label="C", color="black") +
  annotate(geom="text", x=305.5, y=0.06, label="H1: mu=300", color="black")
#ggsave("BF_diagram.svg", device="svg") 


# MBF, SD-MBF, and Bayesianized p-value for He et al. ---------------------------------------------------

z <-  3.57
p <- 2*(1-pnorm(z))
MBF = exp(-z^2/2)
SDMBF =  -exp(1)*p*log(p)

bayes_p <- function(odds, mbf) {
  return (mbf * odds/(1+mbf*odds))
}

x_low = 1
x_high=99

#The values reported in the text for He et al. (2018) 
#can be read off from the plot below, but the monograph
#does not provide this plot

ggplot(data.frame(x = c(x_low,x_high)), aes(x = x)) +
  geom_function(fun = bayes_p, args=list(mbf=SDMBF)) +
  xlab("Odds in favor of null") +
  ylab("Bayesianized p value if SDMBF = 0.0077") 

#ggsave("odds_bayesp.svg", device="svg") 
