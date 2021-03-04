
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using nielsen regions and all time periods                                       
#
# Input:    regression_nreg_level.rda
# Output:   Table XX
#
#====================================================================================================
#====================================================================================================


#====================================================================================================
#                                       Load R packages and data                                          
#====================================================================================================


library(bannerCommenter)
library(tidyverse)
library(dplyr)
#library(stats)
#library(collapse)
library(gridExtra)
library(ggplot2)

library(stargazer)
library(plm)
library(lmtest)
library(scales)
library(jtools)
library(coefplot)
library(ggpubr)
library(mctest)
library(car)

load("./Data_ready/regression_nreg_level.rda")

data <- data_nreg


#====================================================================================================
#                                       Conditioning on 60+
#====================================================================================================


# Drop below 60, low variation in agegroups before (similar to van Baal et al., 2018), lot of noise
data <- data %>% filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+" ) 

#====================================================================================================
#                                       Plotting first differences
#====================================================================================================


# revert to data frame for plotting
data <- as.data.frame(data)


# Generate histogramms and scatterplots for first differences
hist1 <- data %>% 
  filter(d_CVD_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_mort_100k)) +
  geom_histogram() +
  xlim(-200, 200) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist2 <- data %>% 
  filter(d_CVD_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k)) +
  geom_histogram() +
  xlim(-5e+06,5e+06) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist3 <- data %>% 
  filter(d_cancer_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_cancer_mort_100k)) +
  geom_histogram() +
  xlim(-200, 200) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist4 <- data %>% 
  filter(d_cancer_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_cancer_costs_100k)) +
  geom_histogram() +
  xlim(-5e+06,5e+06) +
  stat_central_tendency(type = "median", linetype = "dashed")

scatter1 <- data %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k, y = d_CVD_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

scatter2 <- data %>%
  ggplot(data = data, mapping = aes(x = d_cancer_costs_100k, y = d_cancer_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

# Combine plot
grid.arrange(hist1, hist3, hist2, hist4, scatter1, scatter2)


#====================================================================================================
#                             Run regression using CVD mortality and costs
#====================================================================================================

class(data)
# CVD regressions
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
summary(CVD_lin)

#====================================================================================================
#                                   Testing for multicolinearity
#====================================================================================================


# Regression diagnositcs
par(mfrow=c(2,2))
plot(CVD_lin)

# Test for multicolinearity
vif(CVD_lin)

# If all years included no multicolinearity



CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k + year + ID , data = data)

# Cancer regressions
cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + year + ID , data = data)
cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + year, data = data)
summary(cancer_lin)


cancer_log <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k + year + ID , data = data)

# Creating regression output
states_CVD_lin <- coeftest(CVD_lin, vcov = vcovHC(CVD_lin, type="HC0", cluster="ID"))
states_CVD_log <- coeftest(CVD_log, vcov = vcovHC(CVD_log, type="HC0", cluster="ID"))

states_cancer_lin <- coeftest(cancer_lin, vcov = vcovHC(cancer_lin, type="HC0", cluster="ID"))
states_cancer_log <- coeftest(cancer_log, vcov = vcovHC(cancer_log, type="HC0", cluster="ID"))

states_CVD_lin
states_CVD_log

states_cancer_lin
states_cancer_log


#====================================================================================================
#                                               Final FD model
#====================================================================================================


CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + gender, data = data)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(CVD_lin)

#====================================================================================================
#                                               Final cancer FD model
#====================================================================================================


cancer_reg <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + year + gender, data = data)
summary(cancer_reg)

coeftest(cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(cancer_reg)
vif(cancer_reg)




#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using nielsenreg but only four period model                                     
#
# Input:    regression_nreg_level_four_period.rda
# Output:   Table XX
#
#====================================================================================================
#====================================================================================================

# remove files from the R space
rm(list=ls( ))

load("./Data_ready/regression_nreg_level_four_period.rda")

data <- data_nreg


#====================================================================================================
#                                       Conditioning on 60+
#====================================================================================================


# Drop below 60, low variation in agegroups before (similar to van Baal et al., 2018), lot of noise
data <- data %>% filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+" ) 


#====================================================================================================
#                                       Plotting first differences
#====================================================================================================


# revert to data frame for plotting
data <- as.data.frame(data)

data %>% 
  filter(d_CVD_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_mort_100k)) +
  geom_histogram() +
  stat_central_tendency(type = "median", linetype = "dashed")

# Generate histogramms and scatterplots for first differences
hist1 <- data %>% 
  filter(d_CVD_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_mort_100k)) +
  geom_histogram() +
  xlim(-200, 200) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist2 <- data %>% 
  filter(d_CVD_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k)) +
  geom_histogram() +
  xlim(-5e+06,5e+06) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist3 <- data %>% 
  filter(d_cancer_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_cancer_mort_100k)) +
  geom_histogram() +
  xlim(-200, 200) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist4 <- data %>% 
  filter(d_cancer_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_cancer_costs_100k)) +
  geom_histogram() +
  xlim(-5e+06,5e+06) +
  stat_central_tendency(type = "median", linetype = "dashed")

scatter1 <- data %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k, y = d_CVD_mort_100k, size = population)) +
  geom_point(aes(colour = agegroup)) +
  geom_smooth(method = "lm")
scatter1

scatter2 <- data %>%
  ggplot(data = data, mapping = aes(x = d_cancer_costs_100k, y = d_cancer_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

# Combine plot
grid.arrange(hist1, hist3, hist2, hist4, scatter1, scatter2)


#====================================================================================================
#                             Run regression using CVD mortality and costs
#====================================================================================================

# When using population weights
# https://stackoverflow.com/questions/18260017/how-to-set-a-weighted-least-squares-in-r-for-heteroscedastic-data
# after data: weights = 1/data$population)


#====================================================================================================
#                                   Testing for multicolinearity
#====================================================================================================

# CVD regressions
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k, data = data)

coeftest(CVD_lin)
coeftest(cancer_lin)

summary(CVD_lin)


# Regression diagnositcs
par(mfrow=c(2,2))
plot(CVD_lin)

# Test for multicolinearity, vif of 5 or 10 are seen as problematic
# https://rstudio-pubs-static.s3.amazonaws.com/444662_f07cf3fda4ff4711aafef6546242c410.html#/
vif(CVD_lin)
vif(cancer_lin)
# Lag and d_costs are not highly multicolinear


#====================================================================================================
#                                   Log exploration
#====================================================================================================

# CVD
data %>%
  ggplot(data = data, mapping = aes(x = d_log_CVD_costs_100k, y = d_log_CVD_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

# Cancer
data %>%
  ggplot(data = data, mapping = aes(x = d_log_cancer_costs_100k, y = d_log_cancer_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")


# Check model fit, better with linear model compared to log model, 
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
summary(CVD_lin)
CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k, data = data)
summary(CVD_log)


cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k, data = data)
summary(cancer_lin)
cancer_log <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k, data = data)
summary(cancer_log)


#====================================================================================================
#                                   Testing linear specfication
#====================================================================================================

CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
summary(CVD_lin)
# Test for heteroscedasticity: Breusch-Pagan test
bptest(CVD_lin)

# Not significant, so heteroscedasticity does not seem to be a problem

# Harvey-Collier test for linearity
harvtest(CVD_lin)

# Significant, so assumption is violated, but what is the alternative?

#====================================================================================================
#                                           Models
#====================================================================================================

# 
# CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
# coeftest(CVD_lin)
# 
# 
# 
# CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + ID , data = data)
# coeftest(CVD_lin_ID)
# 
# CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k, data = data)
# coeftest(CVD_log) 
# CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k + ID , data = data)
# coeftest(CVD_log_ID)


#====================================================================================================
#                           Decide on functional form based on model fit
#====================================================================================================

# CVD regressions


summary(CVD_lin)
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year, data = data)
summary(CVD_lin)


CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k, data = data)

# Cancer regressions
cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + gender + year, data = data)
summary(cancer_lin)
cancer_log <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k, data = data)


# Creating regression output
nreg_CVD_lin_4pm <- coeftest(CVD_lin, vcov = vcovHC(CVD_lin, type="HC0", cluster="ID"))
nreg_CVD_log_4pm <- coeftest(CVD_log, vcov = vcovHC(CVD_log, type="HC0", cluster="ID"))

nreg_cancer_lin_4pm <- coeftest(cancer_lin, vcov = vcovHC(cancer_lin, type="HC0", cluster="ID"))
nreg_cancer_log_4pm <- coeftest(cancer_log, vcov = vcovHC(cancer_log, type="HC0", cluster="ID"))


nreg_CVD_lin_4pm
nreg_CVD_log_4pm

nreg_cancer_lin_4pm
nreg_cancer_log_4pm

#====================================================================================================
#                                               Final FD model
#====================================================================================================


CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + gender, data = data)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(CVD_lin)

#====================================================================================================
#                                               Final cancer FD model
#====================================================================================================


cancer_reg <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + year + gender, data = data)
summary(cancer_reg)

coeftest(cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(cancer_reg)
vif(cancer_reg)




#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using nielsenreg but only three period model                                     
#
# Input:    regression_nreg_level_three_period.rda
# Output:   Table XX
#
#====================================================================================================
#====================================================================================================

# remove files from the R space
rm(list=ls( ))

load("./Data_ready/regression_nreg_level_three_period.rda")

data <- data_nreg


#====================================================================================================
#                                       Conditioning on 60+
#====================================================================================================


# Drop below 60, low variation in agegroups before (similar to van Baal et al., 2018), lot of noise
data <- data %>% filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+" ) 


#====================================================================================================
#                                       Plotting first differences
#====================================================================================================


# revert to data frame for plotting
data <- as.data.frame(data)

data %>% 
  filter(d_CVD_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_mort_100k)) +
  geom_histogram() +
  stat_central_tendency(type = "median", linetype = "dashed")

# Generate histogramms and scatterplots for first differences
hist1 <- data %>% 
  filter(d_CVD_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_mort_100k)) +
  geom_histogram() +
  xlim(-200, 200) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist2 <- data %>% 
  filter(d_CVD_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k)) +
  geom_histogram() +
  xlim(-5e+06,5e+06) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist3 <- data %>% 
  filter(d_cancer_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_cancer_mort_100k)) +
  geom_histogram() +
  xlim(-200, 200) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist4 <- data %>% 
  filter(d_cancer_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_cancer_costs_100k)) +
  geom_histogram() +
  xlim(-5e+06,5e+06) +
  stat_central_tendency(type = "median", linetype = "dashed")

scatter1 <- data %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k, y = d_CVD_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

scatter2 <- data %>%
  ggplot(data = data, mapping = aes(x = d_cancer_costs_100k, y = d_cancer_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

# Combine plot, normal distribution unrealistic?
grid.arrange(hist1, hist3, hist2, hist4, scatter1, scatter2)


#====================================================================================================
#                             Run regression using CVD mortality and costs
#====================================================================================================

# When using population weights
# https://stackoverflow.com/questions/18260017/how-to-set-a-weighted-least-squares-in-r-for-heteroscedastic-data
# after data: weights = 1/data$population)


#====================================================================================================
#                                   Testing for multicolinearity
#====================================================================================================

# CVD regressions
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
coeftest(CVD_lin)
summary(CVD_lin)

cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k, data = data)
coeftest(cancer_lin)
summary(cancer_lin)
# Regression diagnositcs
par(mfrow=c(2,2))
plot(CVD_lin)

# Test for multicolinearity, vif of 5 or 10 are seen as problematic
# https://rstudio-pubs-static.s3.amazonaws.com/444662_f07cf3fda4ff4711aafef6546242c410.html#/
vif(CVD_lin)
vif(cancer_lin)
# Lag and d_costs are highly multicolinear for CVD but not for cancer


#====================================================================================================
#                                   Log exploration
#====================================================================================================

# CVD
data %>%
  ggplot(data = data, mapping = aes(x = d_log_CVD_costs_100k, y = d_log_CVD_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

# Cancer
data %>%
  ggplot(data = data, mapping = aes(x = d_log_cancer_costs_100k, y = d_log_cancer_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")


# Check model fit, better with linear model compared to log model, 
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
summary(CVD_lin)
CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k, data = data)
summary(CVD_log)

cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + year + gender, data = data)
summary(cancer_lin)
cancer_log <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k, data = data)
summary(cancer_log)


#====================================================================================================
#                                           Models
#====================================================================================================

# 
# CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
# coeftest(CVD_lin)
# 
# 
# 
# CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + ID , data = data)
# coeftest(CVD_lin_ID)
# 
# CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k, data = data)
# coeftest(CVD_log) 
# CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k + ID , data = data)
# coeftest(CVD_log_ID)



# CVD regressions
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + gender, data = data)
summary(CVD_lin)
CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k, data = data)

# Cancer regressions
cancer_lin <- lm(d_log_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + year + gender, data = data)
summary(cancer_lin)

cancer_log <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k, data = data)


# Creating regression output
nreg_CVD_lin_3pm <- coeftest(CVD_lin, vcov = vcovHC(CVD_lin, type="HC0", cluster="year"))
nreg_CVD_log_3pm <- coeftest(CVD_log, vcov = vcovHC(CVD_log, type="HC0", cluster="year"))

nreg_cancer_lin_3pm <- coeftest(cancer_lin, vcov = vcovHC(cancer_lin, type="HC0", cluster="year"))
nreg_cancer_log_3pm <- coeftest(cancer_log, vcov = vcovHC(cancer_log, type="HC0", cluster="year"))


nreg_CVD_lin_3pm
nreg_CVD_log_3pm


nreg_cancer_log_3pm


#====================================================================================================
#                                               Final FD model
#====================================================================================================


CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + gender, data = data)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(CVD_lin)

#====================================================================================================
#                                               Final cancer FD model
#====================================================================================================


cancer_reg <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + year + gender, data = data)
summary(cancer_reg)

coeftest(cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(cancer_reg)
vif(cancer_reg)






#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using nielsenreg but only two period model (abbr. tpm)                                       
#
# Input:    regression_nreg_level_two_period.rda
# Output:   Table XX
#
#====================================================================================================
#====================================================================================================

# remove files from the R space
rm(list=ls( ))

load("./Data_ready/regression_nreg_level_two_period.rda")

data <- data_wide




#====================================================================================================
#                                       Conditioning on 60+
#====================================================================================================


# Drop below 60, low variation in agegroups before (similar to van Baal et al., 2018), lot of noise
data <- data %>% filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+" ) 

#====================================================================================================
#                                       Plotting first differences
#====================================================================================================


# revert to data frame for plotting
data <- as.data.frame(data)


# Generate histogramms and scatterplots for first differences
hist1 <- data %>% 
  filter(d_CVD_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_mort_100k)) +
  geom_histogram() +
  xlim(-200, 200) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist2 <- data %>% 
  filter(d_CVD_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k)) +
  geom_histogram() +
  xlim(-5e+06,5e+06) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist3 <- data %>% 
  filter(d_cancer_mort_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_cancer_mort_100k)) +
  geom_histogram() +
  xlim(-200, 200) +
  stat_central_tendency(type = "median", linetype = "dashed")

hist4 <- data %>% 
  filter(d_cancer_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_cancer_costs_100k)) +
  geom_histogram() +
  xlim(-5e+06,5e+06) +
  stat_central_tendency(type = "median", linetype = "dashed")

scatter1 <- data %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k, y = d_CVD_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

scatter2 <- data %>%
  ggplot(data = data, mapping = aes(x = d_cancer_costs_100k, y = d_cancer_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

# Combine plot
grid.arrange(hist1, hist3, hist2, hist4, scatter1, scatter2)


#====================================================================================================
#                                               Final CVD FD model
#====================================================================================================

CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + gender, data = data, weights = population_y2012)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(CVD_reg)
vif(CVD_reg)

#====================================================================================================
#                                               Final cancer FD model
#====================================================================================================

cancer_reg <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + gender, data = data, weights = population_y2012)
summary(cancer_reg)

coeftest(cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(cancer_reg)
vif(cancer_reg)


# 
# 
# #====================================================================================================
# #                             Run regression using CVD mortality and costs
# #====================================================================================================
# 
# # When using population weights
# # https://stackoverflow.com/questions/18260017/how-to-set-a-weighted-least-squares-in-r-for-heteroscedastic-data
# # after data: weights = 1/data$population)
# 
# 
# #====================================================================================================
# #                                   Testing for multicolinearity
# #====================================================================================================
# 
# # CVD regressions
# CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
# coeftest(CVD_lin)
# summary(CVD_lin)
# 
# 
# # Regression diagnositcs
# par(mfrow=c(2,2))
# plot(CVD_lin)
# 
# # Test for multicolinearity
# omcdiag(CVD_lin)
# vif(CVD_lin)
# # Farrar Glauber test
# imcdiag(CVD_lin)
# 
# # Lag and d_costs are multicolinear
# 
# # Plotting lag
# 
# scatter_lag_CVD <- data %>%
#   ggplot(data = data, mapping = aes(x = d_lag_CVD_costs_100k, y = d_CVD_mort_100k)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# scatter_lag_CVD
# 
# scatter_lag_CVD_nolag <- data %>%
#   ggplot(data = data, mapping = aes(x = d_lag_CVD_costs_100k, y = d_CVD_costs_100k)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# scatter_lag_CVD_nolag
# 
# 
# scatter_lag_cancer <- data %>%
#   ggplot(data = data, mapping = aes(x = d_lag_cancer_costs_100k, y = d_cancer_mort_100k)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# scatter_lag_cancer
# 
# 
# 
# #====================================================================================================
# #                                           Models
# #====================================================================================================
# 
# 
# 
# 
# CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
# coeftest(CVD_lin)
# 
# 
# 
# CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + ID , data = data)
# coeftest(CVD_lin_ID)
# 
# CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k, data = data)
# coeftest(CVD_log) 
# CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k + ID , data = data)
# coeftest(CVD_log_ID)
# 
# # Cancer regressions
# cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + ID , data = data)
# cancer_log <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k + ID , data = data)
# 
# # Creating regression output
# nreg_CVD_lin_tpm <- coeftest(CVD_lin, vcov = vcovHC(CVD_lin, type="HC0", cluster="ID"))
# nreg_CVD_log_tpm <- coeftest(CVD_log, vcov = vcovHC(CVD_log, type="HC0", cluster="ID"))
# 
# nreg_cancer_lin_tpm <- coeftest(cancer_lin, vcov = vcovHC(cancer_lin, type="HC0", cluster="ID"))
# nreg_cancer_log_tpm <- coeftest(cancer_log, vcov = vcovHC(cancer_log, type="HC0", cluster="ID"))
# 
# 
# nreg_CVD_lin_tpm
# nreg_CVD_log_tpm
# 
# nreg_cancer_lin_tpm
# nreg_cancer_log_tpm
