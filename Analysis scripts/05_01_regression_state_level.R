
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using all states and time periods                                       
#
# Input:    regression_state_level.rda
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
#library(car) # for vif for testing multicolinearity, masks recode from dplyr
library(fixest) # For negbin fixed effects
#library(EnvStats)
library(forecast)
load("./Data_ready/regression_state_level.rda")


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


# CVD regressions
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + ID , data = data)
CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k + year + ID , data = data)

# Cancer regressions
cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + year + ID , data = data)
cancer_log <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k + year + ID , data = data)

# Creating regression output
states_CVD_lin <- coeftest(CVD_lin, vcov = vcovHC(CVD_lin, type="HC0", cluster="ID"))
states_CVD_log <- coeftest(CVD_log, vcov = vcovHC(CVD_log, type="HC0", cluster="ID"))

states_cancer_lin <- coeftest(cancer_lin, vcov = vcovHC(cancer_lin, type="HC0", cluster="ID"))
states_cancer_log <- coeftest(cancer_log, vcov = vcovHC(cancer_log, type="HC0", cluster="ID"))





#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using all states but only four period model                                     
#
# Input:    regression_state_level_four_period.rda
# Output:   Table XX
#
#====================================================================================================
#====================================================================================================

# remove files from the R space
rm(list=ls( ))

load("./Data_ready/regression_state_level_four_period.rda")


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
  xlim(-1000, 500) +
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
#                              Linearity and heteroscedasticity test
#====================================================================================================


# Simplest model
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
summary(CVD_lin)

# No multicolinearity
vif(CVD_lin)

# Test for heteroscedasticity: Breusch-Pagan test
bptest(CVD_lin)

# Significant, so heteroscedasticity seems to be a problem -> use robust standard errors

# Harvey-Collier test for linearity
harvtest(CVD_lin)

# Significant, so assumption is violated, but what is the alternative?

# Regression diagnositcs
# https://data.library.virginia.edu/diagnostic-plots/
par(mfrow=c(2,2))
plot(CVD_lin)

# Signs for heteroscedasticity and non normality, no major outliers found


#====================================================================================================
#                              Specification tests inclusion
#====================================================================================================


# Adding year dummies
CVD_lin_year <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year, data = data)
summary(CVD_lin_year)

# Waldtest and model fit AIC, BIC
waldtest(CVD_lin, CVD_lin_year)
AIC(CVD_lin, CVD_lin_year)
BIC(CVD_lin, CVD_lin_year)

# Year dummies should not be included


# Adding region dummies
CVD_lin_year_region <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + region, data = data)
summary(CVD_lin_year_region)

# Waldtest and model fit AIC, BIC
waldtest(CVD_lin_year, CVD_lin_year_region)
AIC(CVD_lin_year, CVD_lin_year_region)
BIC(CVD_lin_year, CVD_lin_year_region)

# Region dummies do not improve model fit and are jointly not significant

# Adding age x gender dummies
data <- data %>%
  group_by(gender, agegroup) %>%
  mutate(ageXgender = as.factor(cur_group_id()))

CVD_lin_year_ag <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + ageXgender, data = data)
summary(CVD_lin_year_ag)

# Waldtest and model fit AIC, BIC
waldtest(CVD_lin_year, CVD_lin_year_ag)
AIC(CVD_lin_year, CVD_lin_year_ag)
BIC(CVD_lin_year, CVD_lin_year_ag)

# OVerfitting ?

# Age dummies

CVD_lin_year_a <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + agegroup, data = data)
summary(CVD_lin_year_a)

# Waldtest and model fit AIC, BIC
waldtest(CVD_lin_year, CVD_lin_year_a)
AIC(CVD_lin_year, CVD_lin_year_a)
BIC(CVD_lin_year, CVD_lin_year_a)

# Overfitting?

# Gender dummies

CVD_lin_year_g <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + gender, data = data)
summary(CVD_lin_year_g)

# Waldtest and model fit AIC, BIC
waldtest(CVD_lin_year, CVD_lin_year_g)
AIC(CVD_lin_year, CVD_lin_year_g)
BIC(CVD_lin_year, CVD_lin_year_g)

# Is not overfitting, improves mode fit, but also necessary?


#====================================================================================================
#                                               Final FD model
#====================================================================================================

CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + gender, data = data)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(CVD_lin)
#====================================================================================================
#                               Alternative fixed effects model
#====================================================================================================

CVD_reg_FE <- plm(CVD_mort_100k ~ CVD_costs_100k + lag_CVD_costs_100k, 
                    data = data,
                    index = c("ID", "year"), 
                    model = "within", 
                    effect = "twoways")

# print summary using robust standard errors
coeftest(CVD_reg_FE, vcov. = vcovHC, type = "HC1")

summary(CVD_reg_FE)




#====================================================================================================
#                   Alternative fixed effects model with box-cox transformation
#====================================================================================================

#data <- as.data.frame(data)

box_mort <- BoxCox(data$CVD_mort_100k, lambda = "auto")
hist(box_mort)

data <- cbind(data, box_mort)
data <- data %>% 
  mutate(CVD_mort_box = ...32)


CVD_reg_FE_box <- plm(CVD_mort_box ~ CVD_costs_100k + lag_CVD_costs_100k, 
                  data = data,
                  index = c("ID", "year"), 
                  model = "within", 
                  effect = "twoways")

# print summary using robust standard errors
coeftest(CVD_reg_FE_box, vcov. = vcovHC, type = "HC1")

# Box cox transformation does not seem to work


#====================================================================================================
#                                   Alternative Negbin with Fixed effects
#====================================================================================================

# Rationale
hist_mort <- data %>% 
  ggplot(data = data, mapping = aes(x = CVD_mort_100k)) +
  geom_histogram() +
  stat_central_tendency(type = "median", linetype = "dashed")
hist_mort

# Round mortality
data_count <- data %>% 
  mutate(CVD_mort_100k = round(CVD_mort_100k, 0))

# Poisson model
CVD_pois <- fepois(CVD_mort_100k ~ CVD_costs_100k + lag_CVD_costs_100k| ID + year, data_count)
CVD_pois

# Not really sensible results, costs sign positive








# Cancer regressions
cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + ID , data = data)
cancer_log <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k + ID , data = data)

# Creating regression output
states_CVD_lin_tpm <- coeftest(CVD_lin, vcov = vcovHC(CVD_lin, type="HC0", cluster="ID"))
states_CVD_log_tpm <- coeftest(CVD_log, vcov = vcovHC(CVD_log, type="HC0", cluster="ID"))

states_cancer_lin_tpm <- coeftest(cancer_lin, vcov = vcovHC(cancer_lin, type="HC0", cluster="ID"))
states_cancer_log_tpm <- coeftest(cancer_log, vcov = vcovHC(cancer_log, type="HC0", cluster="ID"))


states_CVD_lin_tpm
states_CVD_log_tpm

states_cancer_lin_tpm
states_cancer_log_tpm











#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using all states but only two period model (abbr. tpm)                                       
#
# Input:    regression_state_level_two_period.rda
# Output:   Table XX
#
#====================================================================================================
#====================================================================================================


load("./Data_ready/regression_state_level_two_period.rda")

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
#                             Run regression using CVD mortality and costs
#====================================================================================================


# CVD regressions
CVD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + ID , data = data)
CVD_log <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k + ID , data = data)

# Cancer regressions
cancer_lin <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + ID , data = data)
cancer_log <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k + ID , data = data)

# Creating regression output
states_CVD_lin_tpm <- coeftest(CVD_lin, vcov = vcovHC(CVD_lin, type="HC0", cluster="ID"))
states_CVD_log_tpm <- coeftest(CVD_log, vcov = vcovHC(CVD_log, type="HC0", cluster="ID"))

states_cancer_lin_tpm <- coeftest(cancer_lin, vcov = vcovHC(cancer_lin, type="HC0", cluster="ID"))
states_cancer_log_tpm <- coeftest(cancer_log, vcov = vcovHC(cancer_log, type="HC0", cluster="ID"))


states_CVD_lin_tpm
states_CVD_log_tpm

states_cancer_lin_tpm
states_cancer_log_tpm
