
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis on country level using all time periods                                       
#
# Input:    regression_coutry_level.rda
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

load("./Data_ready/regression_country_level.rda")


#====================================================================================================
#                                       Conditioning on 60+
#====================================================================================================


# Drop below 60, low variation in agegroups before (similar to van Baal et al., 2018), lot of noise
data <- data_wide %>%
  filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+" ) 

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
country_CVD_lin <- coeftest(CVD_lin, vcov = vcovHC(CVD_lin, type="HC0", cluster="ID"))
country_CVD_log <- coeftest(CVD_log, vcov = vcovHC(CVD_log, type="HC0", cluster="ID"))

country_lin <- coeftest(cancer_lin, vcov = vcovHC(cancer_lin, type="HC0", cluster="ID"))
country_cancer_log <- coeftest(cancer_log, vcov = vcovHC(cancer_log, type="HC0", cluster="ID"))

country_CVD_lin
country_CVD_log
country_lin
country_cancer_log