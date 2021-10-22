title: Midterm 
author: Diep Luu
Date: 10/21/2021

library(dplyr)
covid_dat_use <- Household_Pulse_data
attach(covid_dat_use)
na.omit(covid_dat_use)

#Question 1

?na.omit
RECVDVACC[complete.cases(RECVDVACC)]
EEDUC [complete.cases(EEDUC)]

Vaxxed <- (covid_dat_use$RECVDVACC == "yes got vaxx")
assoc.deg <- (covid_dat_use$EEDUC == "assoc deg")
northeast <- (covid_dat_use$REGION == "Northeast")
west <- (covid_dat_use$REGION == "West")

Test1 <- lm(formula = (Vaxxed) ~ (northeast)*(assoc.deg) + (west)*(assoc.deg))
summary(Test1)

#The null hypothesis is not rejected because of the p-value being greater than the significance value of people with 2 year degrees and are from the west.
#The null hypothesis of people with a 2 year degree and from the Northeast would be failed to be rejected because the p-value is above the significance level. 

#Question 2

Male <- (GENID_DESCRIBE == "male") & (RECVDVACC == "yes got vaxx") & (EEDUC == "assoc deg")
female <- (GENID_DESCRIBE == "female") & (RECVDVACC == "yes got vaxx") & (EEDUC == "assoc deg")
vaxx <- (covid_dat_use$RECVDVACC == "yes got vaxx")
 Test2 <- lm(formula = (vaxx) ~ (Male) + (female))
 summary(Test2)
 
 #You fail to reject the null hypothesis for male and female with a 2 year degree because the p-value is higher than the significance level. 

#Question 3
attach(covid_dat_use)
 ?na.omit
summary(covid_dat_use)
summary(covid_dat_use$RRACE)
summary(covid_dat_use$RECVDVACC)
summary(covid_dat_use$works_remote)
summary(covid_dat_use$Works_onsite)

Test3 <- lm(RECVDVACC ~ `EEDUC` + works_remote + `RRACE` + Works_onsite, data = covid_dat_use)
summary(Test3)

NNobs <- length(RRACE)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1) 
COVID_dat_graph <-subset(covid_dat_use,graph_obs)  

plot(RRACE ~ works_remote(RECVDVACC, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2),  data = COVID_dat_graph)
plot(RRACE ~ Works_onsite(RECVDVACC, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,120000), data = COVID_dat_graph)



