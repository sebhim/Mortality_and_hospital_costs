
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on country level and all time periods
#           Similar to study by van Baal et al. (2018)
#
# Input:    clean_data.rda
# Output:   regression_country_level.rda
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

load("./Data_ready/clean_data.rda")


#====================================================================================================
#                                       Reshape for collapsing data 
#====================================================================================================


# Subset data to variables needed
data <- data %>% 
  subset(select = c(year, region, gender, agegroup, CVD_costs_sum, CVD_deaths, cancer_costs_sum, cancer_deaths, population))

# Reshape data to wide format
data_wide <- data %>% 
  pivot_wider(names_from = region, values_from = c(CVD_costs_sum, CVD_deaths, cancer_costs_sum, cancer_deaths, population), names_prefix = "bula_")

# Change censored cancer cost and cases which were censored due to number of cases < 2 to 0 
data_wide <- data_wide %>% 
    mutate(across(contains("_sum_"), ~replace_na(.x, 0)))


#====================================================================================================
#                         Summing up mortality, costs and population
#====================================================================================================

# Combine State costs

# CVD
data_wide <- data_wide %>% 
  mutate(overall_CVD_costs = (CVD_costs_sum_bula_SH +
                            CVD_costs_sum_bula_HH +
                            CVD_costs_sum_bula_NI +
                            CVD_costs_sum_bula_HB +
                            CVD_costs_sum_bula_NW +
                            CVD_costs_sum_bula_HE +
                            CVD_costs_sum_bula_RP +
                            CVD_costs_sum_bula_BW +
                            CVD_costs_sum_bula_BY +
                            CVD_costs_sum_bula_SL +
                            CVD_costs_sum_bula_BE +
                            CVD_costs_sum_bula_BB +
                            CVD_costs_sum_bula_MV +
                            CVD_costs_sum_bula_SN +
                            CVD_costs_sum_bula_ST +
                            CVD_costs_sum_bula_TH))

# Cancer
data_wide <- data_wide %>% 
  mutate(overall_cancer_costs = (cancer_costs_sum_bula_SH +
                               cancer_costs_sum_bula_HH +
                               cancer_costs_sum_bula_NI +
                               cancer_costs_sum_bula_HB +
                               cancer_costs_sum_bula_NW +
                               cancer_costs_sum_bula_HE +
                               cancer_costs_sum_bula_RP +
                               cancer_costs_sum_bula_BW +
                               cancer_costs_sum_bula_BY +
                               cancer_costs_sum_bula_SL +
                               cancer_costs_sum_bula_BE +
                               cancer_costs_sum_bula_BB +
                               cancer_costs_sum_bula_MV +
                               cancer_costs_sum_bula_SN +
                               cancer_costs_sum_bula_ST +
                               cancer_costs_sum_bula_TH))

# Combine State mortality

# CVD
data_wide <- data_wide %>% 
  mutate(overall_CVD_deaths = (CVD_deaths_bula_SH +
                             CVD_deaths_bula_HH +
                             CVD_deaths_bula_NI +
                             CVD_deaths_bula_HB +
                             CVD_deaths_bula_NW +
                             CVD_deaths_bula_HE +
                             CVD_deaths_bula_RP +
                             CVD_deaths_bula_BW +
                             CVD_deaths_bula_BY +
                             CVD_deaths_bula_SL +
                             CVD_deaths_bula_BE +
                             CVD_deaths_bula_BB +
                             CVD_deaths_bula_MV +
                             CVD_deaths_bula_SN +
                             CVD_deaths_bula_ST +
                             CVD_deaths_bula_TH))

# Cancer
data_wide <- data_wide %>% 
  mutate(overall_cancer_deaths = (cancer_deaths_bula_SH +
                                cancer_deaths_bula_HH +
                                cancer_deaths_bula_NI +
                                cancer_deaths_bula_HB +
                                cancer_deaths_bula_NW +
                                cancer_deaths_bula_HE +
                                cancer_deaths_bula_RP +
                                cancer_deaths_bula_BW +
                                cancer_deaths_bula_BY +
                                cancer_deaths_bula_SL +
                                cancer_deaths_bula_BE +
                                cancer_deaths_bula_BB +
                                cancer_deaths_bula_MV +
                                cancer_deaths_bula_SN +
                                cancer_deaths_bula_ST +
                                cancer_deaths_bula_TH))

# Combine population
data_wide <- data_wide %>% 
  mutate(overall_population = (population_bula_SH +
                             population_bula_HH +
                             population_bula_NI +
                             population_bula_HB +
                             population_bula_NW +
                             population_bula_HE +
                             population_bula_RP +
                             population_bula_BW +
                             population_bula_BY +
                             population_bula_SL +
                             population_bula_BE +
                             population_bula_BB +
                             population_bula_MV +
                             population_bula_SN +
                             population_bula_ST +
                             population_bula_TH))


data_wide <- data_wide %>%
  select(year, gender, agegroup, overall_CVD_costs, overall_cancer_costs, overall_CVD_deaths, overall_cancer_deaths, overall_population)


#====================================================================================================
#                                       Generate IDs
#====================================================================================================


# Generate ID for gender x age  groups
data_wide <- data_wide %>%
  group_by(gender, agegroup) %>%
  mutate(ID = cur_group_id())

# Recode ID to be factor
data_wide <- data_wide %>% mutate(ID = as.factor(ID))


#====================================================================================================
#                               Generate mortality and costs per 100k population
#====================================================================================================


# Prepare variables
data_wide <- data_wide %>% 
  mutate(CVD_mort_100k = (overall_CVD_deaths/overall_population)*100000) %>%
  mutate(cancer_mort_100k = (overall_cancer_deaths/overall_population)*100000) %>%
  mutate(CVD_costs_100k = (overall_CVD_costs/overall_population)*100000) %>%
  mutate(cancer_costs_100k = (overall_cancer_costs/overall_population)*100000) %>%
  mutate(log_CVD_mort_100k = log(CVD_mort_100k)) %>%
  mutate(log_cancer_mort_100k = log(cancer_mort_100k)) %>%
  mutate(log_CVD_costs_100k = log(CVD_costs_100k)) %>%
  mutate(log_cancer_costs_100k = log(cancer_costs_100k))


#====================================================================================================
#                                           Panel set data 
#====================================================================================================


# Setting dataset to be panel 
data_wide <- pdata.frame(data_wide, index = c("ID", "year"))

# xtsum panel summary
is.pbalanced(data_wide)
pdim(data_wide)


#====================================================================================================
#                       Create lagged values and individual and time dummies
#====================================================================================================


# Generate lagged cost variables
data_wide <- data_wide %>%
  group_by(ID) %>%
  mutate(lag_log_CVD_costs_100k = lag(log_CVD_costs_100k)) %>% 
  mutate(lag_log_cancer_costs_100k = lag(log_cancer_costs_100k)) %>% 
  mutate(lag_CVD_costs_100k = lag(CVD_costs_100k)) %>% 
  mutate(lag_cancer_costs_100k = lag(cancer_costs_100k))
# 
# # Generate ID dummies
# model.matrix( ~ ID - 1, data_wide)
# data_wide <- data.frame(data_wide, model.matrix( ~ ID - 1, data_wide))
# 
# 
# # Generate year dummies
# model.matrix( ~ year - 1, data_wide)
# data_wide <- data.frame(data_wide, model.matrix( ~ year - 1, data_wide))


#====================================================================================================
#                             Manually creating first differences
#====================================================================================================


# Panel set data frame
data_wide <- pdata.frame(data_wide, index = c("ID", "year"))


# Mortality CVD
data_wide <- data_wide %>% mutate(d_log_CVD_mort_100k = diff(data_wide$log_CVD_mort_100k))
data_wide <- data_wide %>% mutate(d_CVD_mort_100k = diff(data_wide$CVD_mort_100k))

# Costs CVD  
data_wide <- data_wide %>% mutate(d_log_CVD_costs_100k = diff(data_wide$log_CVD_costs_100k))
data_wide <- data_wide %>% mutate(d_CVD_costs_100k = diff(data_wide$CVD_costs_100k))

# Lag costs CVD
data_wide <- data_wide %>% mutate(d_lag_log_CVD_costs_100k = diff(data_wide$lag_log_CVD_costs_100k))
data_wide <- data_wide %>% mutate(d_lag_CVD_costs_100k = diff(data_wide$lag_CVD_costs_100k))


# Mortality cancer
data_wide <- data_wide %>% mutate(d_log_cancer_mort_100k = diff(data_wide$log_cancer_mort_100k))
data_wide <- data_wide %>% mutate(d_cancer_mort_100k = diff(data_wide$cancer_mort_100k))

# Costs cancer  
data_wide <- data_wide %>% mutate(d_log_cancer_costs_100k = diff(data_wide$log_cancer_costs_100k))
data_wide <- data_wide %>% mutate(d_cancer_costs_100k = diff(data_wide$cancer_costs_100k))

# Lag costs cancer
data_wide <- data_wide %>% mutate(d_lag_log_cancer_costs_100k = diff(data_wide$lag_log_cancer_costs_100k))
data_wide <- data_wide %>% mutate(d_lag_cancer_costs_100k = diff(data_wide$lag_cancer_costs_100k))


#====================================================================================================
#                               Save data
#====================================================================================================


# Remove variables not needed for regression analysis
data_wide <- data_wide %>% 
  select(ID, year, gender, agegroup, overall_population,
         d_log_CVD_mort_100k, d_CVD_mort_100k,
         d_log_CVD_costs_100k, d_CVD_costs_100k,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k,
         d_log_cancer_costs_100k, d_cancer_costs_100k,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k) 

save(data_wide, file = "./Data_ready/regression_country_level.rda")

