
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on state level and all time periods                                    
#
# Input:    clean_data.rda
# Output:   regression_state_level.rda
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
#                                 Generate IDs on state level
#====================================================================================================


# Generate ID for gender x age x state groups
data <- data %>%
  group_by(region, gender, agegroup) %>%
  mutate(ID = cur_group_id())

# summarize the variables 'state' and 'year'
summary(data[, c("ID", "year")])

class(data$ID)
class(data$year)

# Recode ID to be factor
data <- data %>% mutate(ID = as.factor(ID))
class(data$ID)


#====================================================================================================
#                           Reduce data set to variables needed
#====================================================================================================


data <- data %>% 
  select(ID, year, region, gender, agegroup,  population,
         log_CVD_mort_100k, CVD_mort_100k, log_CVD_costs_100k, CVD_costs_100k, CVD_costs_sum, CVD_deaths,
         log_cancer_mort_100k, cancer_mort_100k, log_cancer_costs_100k, cancer_costs_100k, cancer_costs_sum, cancer_deaths) 
  

#====================================================================================================
#                           OPTIONAL: USE MORTALITY WHEN LOG 0
#====================================================================================================


# Keep observations with 0 mortality by adding constant 0.00001 and drop columns not needed
# data <- data %>%
#   mutate(log_CVD_mort_100k = recode(log_CVD_mort_100k, "-Inf" = log(10^-5))) %>%
#   mutate(log_cancer_mort_100k = recode(log_cancer_mort_100k, "-Inf" = log(10^-5))) %>%
#   mutate(log_resp_mort_100k = recode(log_resp_mort_100k, "-Inf" = log(10^-5))) %>%
#   mutate(log_overall_mort_100k = recode(log_overall_mort_100k, "-Inf" = log(10^-5)))

# Drop NAs, 2 observations dropped as cancer cost was not available, censored by DRG data holder
# due to number of cases below 5
# data <- data %>%
#   filter(!is.na(log_CVD_costs_100k))
# data <- data %>%
#   filter(!is.na(log_cancer_costs_100k))


#====================================================================================================
#                       Create lagged costs and individual and time dummies
#====================================================================================================


# Setting dataset to be panel 
class(data)
data <- pdata.frame(data, index = c("ID", "year"))

# xtsum panel summary
is.pbalanced(data)
pdim(data)

# Generate lagged cost variables
data <- data %>%
  group_by(ID) %>%
  mutate(lag_log_CVD_costs_100k=lag(log_CVD_costs_100k)) %>% 
  mutate(lag_log_cancer_costs_100k=lag(log_cancer_costs_100k)) %>% 
  mutate(lag_CVD_costs_100k=lag(CVD_costs_100k)) %>% 
  mutate(lag_cancer_costs_100k=lag(cancer_costs_100k))

# # Generate ID dummies
# model.matrix( ~ ID - 1, data)
# data <- data.frame(data, model.matrix( ~ ID - 1, data))
# 
# 
# # Generate year dummies
# model.matrix( ~ year - 1, data)
# data <- data.frame(data, model.matrix( ~ year - 1, data))

data_4pm <- data
data_3pm <- data

#====================================================================================================
#                 Creating first differences for mortality, costs and lagged costs
#====================================================================================================


# Panel set data
data <- pdata.frame(data, index = c("ID", "year"))

# Mortality CVD
data <- data %>% mutate(d_log_CVD_mort_100k = diff(data$log_CVD_mort_100k))
data <- data %>% mutate(d_CVD_mort_100k = diff(data$CVD_mort_100k))

# Costs CVD  
data <- data %>% mutate(d_log_CVD_costs_100k = diff(data$log_CVD_costs_100k))
data <- data %>% mutate(d_CVD_costs_100k = diff(data$CVD_costs_100k))

# Lag costs CVD
data <- data %>% mutate(d_lag_log_CVD_costs_100k = diff(data$lag_log_CVD_costs_100k))
data <- data %>% mutate(d_lag_CVD_costs_100k = diff(data$lag_CVD_costs_100k))

# Mortality cancer
data <- data %>% mutate(d_log_cancer_mort_100k = diff(data$log_cancer_mort_100k))
data <- data %>% mutate(d_cancer_mort_100k = diff(data$cancer_mort_100k))

# Costs cancer  
data <- data %>% mutate(d_log_cancer_costs_100k = diff(data$log_cancer_costs_100k))
data <- data %>% mutate(d_cancer_costs_100k = diff(data$cancer_costs_100k))

# Lag costs cancer
data <- data %>% mutate(d_lag_log_cancer_costs_100k = diff(data$lag_log_cancer_costs_100k))
data <- data %>% mutate(d_lag_cancer_costs_100k = diff(data$lag_cancer_costs_100k))



#====================================================================================================
#                               Save data
#====================================================================================================


# Remove variables not needed for regression analysis
data <- data %>% 
  select(ID, year, region, gender, agegroup, population,
         d_log_CVD_mort_100k, d_CVD_mort_100k,
         d_log_CVD_costs_100k, d_CVD_costs_100k,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k,
         d_log_cancer_costs_100k, d_cancer_costs_100k,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k) 

save(data, file = "./Data_ready/regression_state_level.rda")




#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on state level but only four time periods                                    
#
# Input:    data_4pm
# Output:   regression_state_level_four_period.rda
#
#====================================================================================================
#====================================================================================================


data <- data_4pm



#====================================================================================================
#                             Condition on four periods and recode
#====================================================================================================

class(data)
data <- data %>% 
  filter(year == "2008" | year == "2011" | year == "2014" | year == "2017")

class(data$year)

data <- data %>% 
  mutate(year = recode(year,
                              "2008" = "1",
                              "2011" = "2",
                              "2014" = "3",
                              "2017" = "4"))


#====================================================================================================
#                             Manually creating first differences
#====================================================================================================


# Panel set data frame
data <- pdata.frame(data, index = c("ID", "year"))


# Mortality CVD
data <- data %>% mutate(d_log_CVD_mort_100k = diff(data$log_CVD_mort_100k))
data <- data %>% mutate(d_CVD_mort_100k = diff(data$CVD_mort_100k))

# Costs CVD  
data <- data %>% mutate(d_log_CVD_costs_100k = diff(data$log_CVD_costs_100k))
data <- data %>% mutate(d_CVD_costs_100k = diff(data$CVD_costs_100k))

# Lag costs CVD
data <- data %>% mutate(d_lag_log_CVD_costs_100k = diff(data$lag_log_CVD_costs_100k))
data <- data %>% mutate(d_lag_CVD_costs_100k = diff(data$lag_CVD_costs_100k))

# CVD costs squared
data <- data %>% mutate(CVD_costs_100k_sq = CVD_costs_100k^2)
data <- data %>% mutate(d_CVD_costs_100k_sq = diff(data$CVD_costs_100k_sq))

data <- data %>% mutate(lag_CVD_costs_100k_sq = lag_CVD_costs_100k^2)
data <- data %>% mutate(d_lag_CVD_costs_100k_sq = diff(data$lag_CVD_costs_100k_sq))



# Mortality cancer
data <- data %>% mutate(d_log_cancer_mort_100k = diff(data$log_cancer_mort_100k))
data <- data %>% mutate(d_cancer_mort_100k = diff(data$cancer_mort_100k))

# Costs cancer  
data <- data %>% mutate(d_log_cancer_costs_100k = diff(data$log_cancer_costs_100k))
data <- data %>% mutate(d_cancer_costs_100k = diff(data$cancer_costs_100k))

# Lag costs cancer
data <- data %>% mutate(d_lag_log_cancer_costs_100k = diff(data$lag_log_cancer_costs_100k))
data <- data %>% mutate(d_lag_cancer_costs_100k = diff(data$lag_cancer_costs_100k))


#====================================================================================================
#                             Manually creating first differences per capita
#====================================================================================================

# Mortality CVD pc
data <- data %>% mutate(d_log_CVD_mort_pc = diff(log(data$CVD_mort_100k/100000)))

# Costs CVD  pc
data <- data %>% mutate(d_log_CVD_costs_pc = diff(log(data$CVD_costs_100k/100000)))

# Produces same results when not per 100,000 but per capita for log first differences

#====================================================================================================
#                               Save data
#====================================================================================================


# Remove variables not needed for regression analysis
data <- data %>% 
  select(ID, year, gender, agegroup, region, population,
         d_log_CVD_mort_100k, log_CVD_mort_100k, d_CVD_mort_100k, CVD_mort_100k, CVD_deaths,
         d_log_CVD_costs_100k, log_CVD_costs_100k, d_CVD_costs_100k, CVD_costs_100k, CVD_costs_100k_sq, d_CVD_costs_100k_sq, CVD_costs_sum,
         d_lag_log_CVD_costs_100k, lag_log_CVD_costs_100k, d_lag_CVD_costs_100k, lag_CVD_costs_100k, lag_CVD_costs_100k_sq, d_lag_CVD_costs_100k_sq,
         d_log_cancer_mort_100k, log_cancer_mort_100k, d_cancer_mort_100k, cancer_mort_100k,
         d_log_cancer_costs_100k, log_cancer_costs_100k, d_cancer_costs_100k, cancer_costs_100k, cancer_costs_sum, cancer_deaths,
         d_lag_log_cancer_costs_100k, lag_log_cancer_costs_100k, d_lag_cancer_costs_100k, lag_cancer_costs_100k) 

save(data, file = "./Data_ready/regression_state_level_four_period.rda")



#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on state level but only four time periods                                    
#
# Input:    data_3pm
# Output:   regression_state_level_three_period.rda
#
#====================================================================================================
#====================================================================================================


data <- data_3pm



#====================================================================================================
#                             Condition on three periods and recode
#====================================================================================================

class(data)
data <- data %>% 
  filter(year == "2008" | year == "2012" | year == "2017")

class(data$year)

data <- data %>% 
  mutate(year = recode(year,
                       "2008" = "1",
                       "2012" = "2",
                       "2017" = "3"))


#====================================================================================================
#                             Manually creating first differences
#====================================================================================================


# Panel set data frame
data <- pdata.frame(data, index = c("ID", "year"))


# Mortality CVD
data <- data %>% mutate(d_log_CVD_mort_100k = diff(data$log_CVD_mort_100k))
data <- data %>% mutate(d_CVD_mort_100k = diff(data$CVD_mort_100k))

# Costs CVD  
data <- data %>% mutate(d_log_CVD_costs_100k = diff(data$log_CVD_costs_100k))
data <- data %>% mutate(d_CVD_costs_100k = diff(data$CVD_costs_100k))

# Lag costs CVD
data <- data %>% mutate(d_lag_log_CVD_costs_100k = diff(data$lag_log_CVD_costs_100k))
data <- data %>% mutate(d_lag_CVD_costs_100k = diff(data$lag_CVD_costs_100k))

# CVD costs squared
data <- data %>% mutate(CVD_costs_100k_sq = CVD_costs_100k^2)
data <- data %>% mutate(d_CVD_costs_100k_sq = diff(data$CVD_costs_100k_sq))

data <- data %>% mutate(lag_CVD_costs_100k_sq = lag_CVD_costs_100k^2)
data <- data %>% mutate(d_lag_CVD_costs_100k_sq = diff(data$lag_CVD_costs_100k_sq))



# Mortality cancer
data <- data %>% mutate(d_log_cancer_mort_100k = diff(data$log_cancer_mort_100k))
data <- data %>% mutate(d_cancer_mort_100k = diff(data$cancer_mort_100k))

# Costs cancer  
data <- data %>% mutate(d_log_cancer_costs_100k = diff(data$log_cancer_costs_100k))
data <- data %>% mutate(d_cancer_costs_100k = diff(data$cancer_costs_100k))

# Lag costs cancer
data <- data %>% mutate(d_lag_log_cancer_costs_100k = diff(data$lag_log_cancer_costs_100k))
data <- data %>% mutate(d_lag_cancer_costs_100k = diff(data$lag_cancer_costs_100k))

#====================================================================================================
#                               Save data
#====================================================================================================


# Remove variables not needed for regression analysis
data <- data %>% 
  select(ID, year, gender, agegroup, region, population,
         d_log_CVD_mort_100k, log_CVD_mort_100k, d_CVD_mort_100k, CVD_mort_100k,
         d_log_CVD_costs_100k, log_CVD_costs_100k, d_CVD_costs_100k, CVD_costs_100k, CVD_costs_100k_sq, d_CVD_costs_100k_sq,
         d_lag_log_CVD_costs_100k, lag_log_CVD_costs_100k, d_lag_CVD_costs_100k, lag_CVD_costs_100k, lag_CVD_costs_100k_sq, d_lag_CVD_costs_100k_sq,
         d_log_cancer_mort_100k, log_cancer_mort_100k, d_cancer_mort_100k, cancer_mort_100k,
         d_log_cancer_costs_100k, log_cancer_costs_100k, d_cancer_costs_100k, cancer_costs_100k,
         d_lag_log_cancer_costs_100k, lag_log_cancer_costs_100k, d_lag_cancer_costs_100k, lag_cancer_costs_100k) 

save(data, file = "./Data_ready/regression_state_level_three_period.rda")



#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on state level but only two time periods                                    
#
# Input:    clean_data.rda
# Output:   regression_state_level_two_period.rda
#
#====================================================================================================
#====================================================================================================


#====================================================================================================
#                                       Load data                                          
#====================================================================================================


load("./Data_ready/clean_data.rda")


#====================================================================================================
#                                 Generate IDs on age/gender level
#====================================================================================================


# Generate ID for gender x age 
data <- data %>%
  group_by(gender, agegroup) %>%
  mutate(ID = cur_group_id())

# ID as factor variable
data <- data %>%
  mutate(ID = as.factor(ID))


#====================================================================================================
#                 Reshape to wide and then calculate differences between 2008 and 2017
#                 and their corresponding lags (2007 and 2016)
#====================================================================================================


# Reshape data to wide format
data <- data %>% 
  subset(select = c(ID, year, region, gender,agegroup, population,
                    log_CVD_mort_100k, log_CVD_costs_100k, CVD_mort_100k, CVD_costs_100k,
                    log_cancer_mort_100k, log_cancer_costs_100k, cancer_mort_100k, cancer_costs_100k))

data_wide <- data %>%
  pivot_wider(names_from = year, values_from = c(population, log_CVD_mort_100k, log_CVD_costs_100k, 
                                                 CVD_mort_100k, CVD_costs_100k,
                                                 log_cancer_mort_100k, log_cancer_costs_100k, 
                                                 cancer_mort_100k, cancer_costs_100k), values_fill = NA, names_prefix = "y")


#====================================================================================================
#                        Generate first differences of non log, log and lags
#====================================================================================================

# Generate first differences and their lags CVD
data_wide <- data_wide %>%
  mutate(d_log_CVD_mort_100k = log_CVD_mort_100k_y2017 - log_CVD_mort_100k_y2008) %>% 
  mutate(d_log_CVD_costs_100k = log_CVD_costs_100k_y2017 - log_CVD_costs_100k_y2008) %>% 
  mutate(d_lag_log_CVD_costs_100k = log_CVD_costs_100k_y2016 - log_CVD_costs_100k_y2007) %>% 
  mutate(d_CVD_mort_100k = CVD_mort_100k_y2017 - CVD_mort_100k_y2008) %>% 
  mutate(d_CVD_costs_100k = CVD_costs_100k_y2017 - CVD_costs_100k_y2008) %>% 
  mutate(d_lag_CVD_costs_100k = CVD_costs_100k_y2016 - CVD_costs_100k_y2007)


# Generate first differences and their lags cancer
data_wide <- data_wide %>%
  mutate(d_log_cancer_mort_100k = log_cancer_mort_100k_y2017 - log_cancer_mort_100k_y2008) %>% 
  mutate(d_log_cancer_costs_100k = log_cancer_costs_100k_y2017 - log_cancer_costs_100k_y2008) %>% 
  mutate(d_lag_log_cancer_costs_100k = log_cancer_costs_100k_y2016 - log_cancer_costs_100k_y2007) %>% 
  mutate(d_cancer_mort_100k = cancer_mort_100k_y2017 - cancer_mort_100k_y2008) %>% 
  mutate(d_cancer_costs_100k = cancer_costs_100k_y2017 - cancer_costs_100k_y2008) %>% 
  mutate(d_lag_cancer_costs_100k = cancer_costs_100k_y2016 - cancer_costs_100k_y2007)


#====================================================================================================
#                               Save data
#====================================================================================================


# Remove variables not needed for regression analysis, using population distribution at midpoint of 
# 2007 to 2017 for weighting regression

data_wide <- data_wide %>% 
  select(ID, region, gender, agegroup, population_y2012,
         d_log_CVD_mort_100k, d_CVD_mort_100k,
         d_log_CVD_costs_100k, d_CVD_costs_100k,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k,
         d_log_cancer_costs_100k, d_cancer_costs_100k,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k) 

save(data_wide, file = "./Data_ready/regression_state_level_two_period.rda")

