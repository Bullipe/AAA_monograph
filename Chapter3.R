library(tidyverse)
library(ggtext)

# Code for Figure 3.1------------
#parameters
mu1 = 300
mu2=290
n=25
sd = 30

#calculations
se= sd/sqrt(n)
x_crit = qnorm(0.10,mu1,se)
power = pnorm(x_crit,mu2,se) 
power #works out to 65%, used for label below

#graph x limits
x_low = 278
x_high = 312


ggplot(data.frame(x = c(x_low,x_high)), aes(x = x)) +
  geom_area(stat = "function", fun = dnorm, args=list(mean=mu1,sd=se), fill = "#00998a", xlim = c(x_low, x_crit), alpha=0.50) +   
  geom_area(stat = "function", fun = dnorm, args=list(mean=mu2,sd=se), fill = "grey70", xlim = c(x_low, x_crit), alpha=0.5) +   
  stat_function(fun = dnorm, args=list(mean=mu2,sd=se)) +
  stat_function(fun = dnorm, args=list(mean=mu1,sd=se)) +
  xlab("milk powder quantity (g)") +
  ylab("Probability density of sample mean under H1 and H2") +
  annotate(geom="text", x=289, y=0.05, label="Power = 65%", color="black") +
  annotate(geom="text", x=290, y=0.006, label="Type I\nerror = 10%", color="black") +
  annotate(geom="text", x=292.3, y=-0.003, label="292.3", color="black") +
  annotate(geom="text", x=284.7, y=0.06, label="H2: \u03BC=290", color="black") + #\u03BC=mu
  annotate(geom="text", x=305.5, y=0.06, label="H1: \u03BC=300", color="black")

ggsave("power_diagram.png", device="png") 


