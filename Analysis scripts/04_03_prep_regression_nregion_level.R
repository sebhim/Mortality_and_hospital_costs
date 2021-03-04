
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on Nielssen region level
#           to reduce noise in the data due to very small states.
#
# Input:    clean_data.rda
# Output:   regression_nreg_level.rda
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
#               Summing up mortality, costs and population across Nielsen regions
# Defined also based on ecnomic structure/similarity: https://de.zxc.wiki/wiki/Nielsengebiet
#====================================================================================================

# Nielsen 1: HB, HH, NI, SH
# Nielsen 2: NW
# Nielsen 3: HE, RP, SL
# Nielsen 4: BW
# Nielsen 5: BY
# Nielsen 6: BE, BB, MV, ST
# Nielsen 7: SN, TH


#====================================================================================================
#                           Combine costs to Nielsen regions
#====================================================================================================


# CVD
data_wide <- data_wide %>% 
  mutate(N1_CVD_costs_sum = (CVD_costs_sum_bula_SH +
                            CVD_costs_sum_bula_HH +
                            CVD_costs_sum_bula_NI +
                            CVD_costs_sum_bula_HB)) %>% 
  mutate(N2_CVD_costs_sum = (CVD_costs_sum_bula_NW)) %>% 
  mutate(N3_CVD_costs_sum = (CVD_costs_sum_bula_HE +
                               CVD_costs_sum_bula_RP +
                               CVD_costs_sum_bula_SL)) %>%
  mutate(N4_CVD_costs_sum = (CVD_costs_sum_bula_BW)) %>%
  mutate(N5_CVD_costs_sum = (CVD_costs_sum_bula_BY)) %>%
  mutate(N6_CVD_costs_sum = (CVD_costs_sum_bula_BE +
                               CVD_costs_sum_bula_BB +
                               CVD_costs_sum_bula_MV +
                               CVD_costs_sum_bula_ST)) %>%
  mutate(N7_CVD_costs_sum = (CVD_costs_sum_bula_SN +
                               CVD_costs_sum_bula_TH))  

# Cancer
data_wide <- data_wide %>% 
  mutate(N1_cancer_costs_sum = (cancer_costs_sum_bula_SH +
                               cancer_costs_sum_bula_HH +
                               cancer_costs_sum_bula_NI +
                               cancer_costs_sum_bula_HB)) %>% 
  mutate(N2_cancer_costs_sum = (cancer_costs_sum_bula_NW)) %>% 
  mutate(N3_cancer_costs_sum = (cancer_costs_sum_bula_HE +
                               cancer_costs_sum_bula_RP +
                               cancer_costs_sum_bula_SL)) %>%
  mutate(N4_cancer_costs_sum = (cancer_costs_sum_bula_BW)) %>%
  mutate(N5_cancer_costs_sum = (cancer_costs_sum_bula_BY)) %>%
  mutate(N6_cancer_costs_sum = (cancer_costs_sum_bula_BE +
                               cancer_costs_sum_bula_BB +
                               cancer_costs_sum_bula_MV +
                               cancer_costs_sum_bula_ST)) %>%
  mutate(N7_cancer_costs_sum = (cancer_costs_sum_bula_SN +
                               cancer_costs_sum_bula_TH))  


#====================================================================================================
#                           Combine mortality to Nielsen regions
#====================================================================================================


# CVD
data_wide <- data_wide %>% 
  mutate(N1_CVD_deaths = (CVD_deaths_bula_SH +
                               CVD_deaths_bula_HH +
                               CVD_deaths_bula_NI +
                               CVD_deaths_bula_HB)) %>% 
  mutate(N2_CVD_deaths = (CVD_deaths_bula_NW)) %>% 
  mutate(N3_CVD_deaths = (CVD_deaths_bula_HE +
                               CVD_deaths_bula_RP +
                               CVD_deaths_bula_SL)) %>%
  mutate(N4_CVD_deaths = (CVD_deaths_bula_BW)) %>%
  mutate(N5_CVD_deaths = (CVD_deaths_bula_BY)) %>%
  mutate(N6_CVD_deaths = (CVD_deaths_bula_BE +
                               CVD_deaths_bula_BB +
                               CVD_deaths_bula_MV +
                               CVD_deaths_bula_ST)) %>%
  mutate(N7_CVD_deaths = (CVD_deaths_bula_SN +
                               CVD_deaths_bula_TH))  

# Cancer
data_wide <- data_wide %>% 
  mutate(N1_cancer_deaths = (cancer_deaths_bula_SH +
                               cancer_deaths_bula_HH +
                               cancer_deaths_bula_NI +
                               cancer_deaths_bula_HB)) %>% 
  mutate(N2_cancer_deaths = (cancer_deaths_bula_NW)) %>% 
  mutate(N3_cancer_deaths = (cancer_deaths_bula_HE +
                               cancer_deaths_bula_RP +
                               cancer_deaths_bula_SL)) %>%
  mutate(N4_cancer_deaths = (cancer_deaths_bula_BW)) %>%
  mutate(N5_cancer_deaths = (cancer_deaths_bula_BY)) %>%
  mutate(N6_cancer_deaths = (cancer_deaths_bula_BE +
                               cancer_deaths_bula_BB +
                               cancer_deaths_bula_MV +
                               cancer_deaths_bula_ST)) %>%
  mutate(N7_cancer_deaths = (cancer_deaths_bula_SN +
                               cancer_deaths_bula_TH))  

#====================================================================================================
#                           Combine population to Nielsen regions
#====================================================================================================


data_wide <- data_wide %>% 
  mutate(N1_population = (population_bula_SH +
                               population_bula_HH +
                               population_bula_NI +
                               population_bula_HB)) %>% 
  mutate(N2_population = (population_bula_NW)) %>% 
  mutate(N3_population = (population_bula_HE +
                               population_bula_RP +
                               population_bula_SL)) %>%
  mutate(N4_population = (population_bula_BW)) %>%
  mutate(N5_population = (population_bula_BY)) %>%
  mutate(N6_population = (population_bula_BE +
                               population_bula_BB +
                               population_bula_MV +
                               population_bula_ST)) %>%
  mutate(N7_population = (population_bula_SN +
                               population_bula_TH))  


#====================================================================================================
#           Generate long reshaped vectors of costs, mortality and population
#====================================================================================================


# CVD costs
CVD_costs <- data_wide %>%
  select(year, gender, agegroup, ends_with("CVD_costs_sum")) %>% 
  pivot_longer(ends_with("CVD_costs_sum"), names_to = "region", values_to = "CVD_costs_sum") %>% 
  mutate(nielsenreg = substr(region, start = 2, stop = 2)) %>% 
  select(-region) %>%
  group_by(gender, agegroup, nielsenreg) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# Cancer costs
cancer_costs <- data_wide %>%
  select(year, gender, agegroup, ends_with("cancer_costs_sum")) %>% 
  pivot_longer(ends_with("cancer_costs_sum"), names_to = "region", values_to = "cancer_costs_sum") %>% 
  mutate(nielsenreg = substr(region, start = 2, stop = 2)) %>% 
  select(-region) %>%
  group_by(gender, agegroup, nielsenreg) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# CVD deaths
CVD_deaths <- data_wide %>%
  select(year, gender, agegroup, ends_with("CVD_deaths")) %>% 
  pivot_longer(ends_with("CVD_deaths"), names_to = "region", values_to = "CVD_deaths") %>% 
  mutate(nielsenreg = substr(region, start = 2, stop = 2)) %>% 
  select(-region) %>%
  group_by(gender, agegroup, nielsenreg) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# cancer deaths
cancer_deaths <- data_wide %>%
  select(year, gender, agegroup, ends_with("cancer_deaths")) %>% 
  pivot_longer(ends_with("cancer_deaths"), names_to = "region", values_to = "cancer_deaths") %>% 
  mutate(nielsenreg = substr(region, start = 2, stop = 2)) %>% 
  select(-region) %>%
  group_by(gender, agegroup, nielsenreg) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# Population
population <- data_wide %>%
  select(year, gender, agegroup, ends_with("population")) %>% 
  pivot_longer(ends_with("population"), names_to = "region", values_to = "population") %>% 
  mutate(nielsenreg = substr(region, start = 2, stop = 2)) %>% 
  select(-region) %>%
  group_by(gender, agegroup, nielsenreg) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))


#====================================================================================================
#                 Merge reshaped vectors of costs, mortality and population
#====================================================================================================


data_nreg <- inner_join(cancer_costs, cancer_deaths, by= NULL) %>% 
  inner_join(., CVD_costs, by= NULL) %>% 
  inner_join(., CVD_deaths, by= NULL) %>% 
  inner_join(., population, by= NULL)


#====================================================================================================
#                               Generate mortality and costs per 100k population
#====================================================================================================


# Prepare variables
data_nreg <- data_nreg %>% 
  mutate(CVD_mort_100k = (CVD_deaths/population)*100000) %>%
  mutate(cancer_mort_100k = (cancer_deaths/population)*100000) %>%
  mutate(CVD_costs_100k = (CVD_costs_sum/population)*100000) %>%
  mutate(cancer_costs_100k = (cancer_costs_sum/population)*100000) %>%
  mutate(log_CVD_mort_100k = log(CVD_mort_100k)) %>%
  mutate(log_cancer_mort_100k = log(cancer_mort_100k)) %>%
  mutate(log_CVD_costs_100k = log(CVD_costs_100k)) %>%
  mutate(log_cancer_costs_100k = log(cancer_costs_100k))


#====================================================================================================
#                                           Panel set data 
#====================================================================================================


# Setting dataset to be panel 
data_nreg <- pdata.frame(data_nreg, index = c("ID", "year"))

# xtsum panel summary
is.pbalanced(data_nreg)
pdim(data_nreg)


#====================================================================================================
#                       Create lagged values and individual and time dummies
#====================================================================================================


# Generate lagged cost variables
data_nreg <- data_nreg %>%
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

# Save data frame for two period model and four period models
data_long <- data_nreg

#====================================================================================================
#                             Manually creating first differences
#====================================================================================================


# Panel set data frame
data_nreg <- pdata.frame(data_nreg, index = c("ID", "year"))


# Mortality CVD
data_nreg <- data_nreg %>% mutate(d_log_CVD_mort_100k = diff(data_nreg$log_CVD_mort_100k))
data_nreg <- data_nreg %>% mutate(d_CVD_mort_100k = diff(data_nreg$CVD_mort_100k))

# Costs CVD  
data_nreg <- data_nreg %>% mutate(d_log_CVD_costs_100k = diff(data_nreg$log_CVD_costs_100k))
data_nreg <- data_nreg %>% mutate(d_CVD_costs_100k = diff(data_nreg$CVD_costs_100k))

# Lag costs CVD
data_nreg <- data_nreg %>% mutate(d_lag_log_CVD_costs_100k = diff(data_nreg$lag_log_CVD_costs_100k))
data_nreg <- data_nreg %>% mutate(d_lag_CVD_costs_100k = diff(data_nreg$lag_CVD_costs_100k))


# Mortality cancer
data_nreg <- data_nreg %>% mutate(d_log_cancer_mort_100k = diff(data_nreg$log_cancer_mort_100k))
data_nreg <- data_nreg %>% mutate(d_cancer_mort_100k = diff(data_nreg$cancer_mort_100k))

# Costs cancer  
data_nreg <- data_nreg %>% mutate(d_log_cancer_costs_100k = diff(data_nreg$log_cancer_costs_100k))
data_nreg <- data_nreg %>% mutate(d_cancer_costs_100k = diff(data_nreg$cancer_costs_100k))

# Lag costs cancer
data_nreg <- data_nreg %>% mutate(d_lag_log_cancer_costs_100k = diff(data_nreg$lag_log_cancer_costs_100k))
data_nreg <- data_nreg %>% mutate(d_lag_cancer_costs_100k = diff(data_nreg$lag_cancer_costs_100k))


#====================================================================================================
#                               Save data
#====================================================================================================


# Remove variables not needed for regression analysis
data_nreg <- data_nreg %>% 
  select(ID, year, gender, agegroup, nielsenreg, population,
         d_log_CVD_mort_100k, d_CVD_mort_100k,
         d_log_CVD_costs_100k, d_CVD_costs_100k,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k,
         d_log_cancer_costs_100k, d_cancer_costs_100k,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k) 

save(data_nreg, file = "./Data_ready/regression_nreg_level.rda")






#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on nieslsen region level but only four time periods                                    
#
# Input:    data_long
# Output:   regression_nreg_level_four_period.rda
#
#====================================================================================================
#====================================================================================================

data_nreg <- data_long

#====================================================================================================
#                             Condition on four periods and recode
#====================================================================================================

class(data_nreg)
data_nreg <- data_nreg %>% 
  filter(year == "2008" | year == "2011" | year == "2014" | year == "2017")

class(data_nreg$year)

data_nreg <- data_nreg %>% 
  dplyr::mutate(year = recode(year,
                       "2008" = "1",
                       "2011" = "2",
                       "2014" = "3",
                       "2017" = "4"))


#====================================================================================================
#                             Manually creating first differences
#====================================================================================================


# Panel set data frame
data_nreg <- pdata.frame(data_nreg, index = c("ID", "year"))


# Mortality CVD
data_nreg <- data_nreg %>% mutate(d_log_CVD_mort_100k = diff(data_nreg$log_CVD_mort_100k))
data_nreg <- data_nreg %>% mutate(d_CVD_mort_100k = diff(data_nreg$CVD_mort_100k))

# Costs CVD  
data_nreg <- data_nreg %>% mutate(d_log_CVD_costs_100k = diff(data_nreg$log_CVD_costs_100k))
data_nreg <- data_nreg %>% mutate(d_CVD_costs_100k = diff(data_nreg$CVD_costs_100k))

# Lag costs CVD
data_nreg <- data_nreg %>% mutate(d_lag_log_CVD_costs_100k = diff(data_nreg$lag_log_CVD_costs_100k))
data_nreg <- data_nreg %>% mutate(d_lag_CVD_costs_100k = diff(data_nreg$lag_CVD_costs_100k))


# Mortality cancer
data_nreg <- data_nreg %>% mutate(d_log_cancer_mort_100k = diff(data_nreg$log_cancer_mort_100k))
data_nreg <- data_nreg %>% mutate(d_cancer_mort_100k = diff(data_nreg$cancer_mort_100k))

# Costs cancer  
data_nreg <- data_nreg %>% mutate(d_log_cancer_costs_100k = diff(data_nreg$log_cancer_costs_100k))
data_nreg <- data_nreg %>% mutate(d_cancer_costs_100k = diff(data_nreg$cancer_costs_100k))

# Lag costs cancer
data_nreg <- data_nreg %>% mutate(d_lag_log_cancer_costs_100k = diff(data_nreg$lag_log_cancer_costs_100k))
data_nreg <- data_nreg %>% mutate(d_lag_cancer_costs_100k = diff(data_nreg$lag_cancer_costs_100k))


#====================================================================================================
#                               Save data
#====================================================================================================


# Remove variables not needed for regression analysis
data_nreg <- data_nreg %>% 
  select(ID, year, gender, agegroup, nielsenreg, population,
         d_log_CVD_mort_100k, d_CVD_mort_100k, CVD_deaths,
         d_log_CVD_costs_100k, d_CVD_costs_100k, CVD_costs_sum,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k, cancer_deaths,
         d_log_cancer_costs_100k, d_cancer_costs_100k, cancer_costs_sum,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k) 

save(data_nreg, file = "./Data_ready/regression_nreg_level_four_period.rda")





#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on nieslsen region level but only three time periods                                    
#
# Input:    data_long
# Output:   regression_nreg_level_three_period.rda
#
#====================================================================================================
#====================================================================================================

data_nreg <- data_long

#====================================================================================================
#                             Condition on three periods and recode
#====================================================================================================

class(data_nreg)
data_nreg <- data_nreg %>% 
  filter(year == "2008" | year == "2012" | year == "2017")

class(data_nreg$year)

data_nreg <- data_nreg %>% 
  dplyr::mutate(year = recode(year,
                              "2008" = "1",
                              "2012" = "2",
                              "2017" = "3",
                              ))


#====================================================================================================
#                             Manually creating first differences
#====================================================================================================


# Panel set data frame
data_nreg <- pdata.frame(data_nreg, index = c("ID", "year"))


# Mortality CVD
data_nreg <- data_nreg %>% mutate(d_log_CVD_mort_100k = diff(data_nreg$log_CVD_mort_100k))
data_nreg <- data_nreg %>% mutate(d_CVD_mort_100k = diff(data_nreg$CVD_mort_100k))

# Costs CVD  
data_nreg <- data_nreg %>% mutate(d_log_CVD_costs_100k = diff(data_nreg$log_CVD_costs_100k))
data_nreg <- data_nreg %>% mutate(d_CVD_costs_100k = diff(data_nreg$CVD_costs_100k))

# Lag costs CVD
data_nreg <- data_nreg %>% mutate(d_lag_log_CVD_costs_100k = diff(data_nreg$lag_log_CVD_costs_100k))
data_nreg <- data_nreg %>% mutate(d_lag_CVD_costs_100k = diff(data_nreg$lag_CVD_costs_100k))


# Mortality cancer
data_nreg <- data_nreg %>% mutate(d_log_cancer_mort_100k = diff(data_nreg$log_cancer_mort_100k))
data_nreg <- data_nreg %>% mutate(d_cancer_mort_100k = diff(data_nreg$cancer_mort_100k))

# Costs cancer  
data_nreg <- data_nreg %>% mutate(d_log_cancer_costs_100k = diff(data_nreg$log_cancer_costs_100k))
data_nreg <- data_nreg %>% mutate(d_cancer_costs_100k = diff(data_nreg$cancer_costs_100k))

# Lag costs cancer
data_nreg <- data_nreg %>% mutate(d_lag_log_cancer_costs_100k = diff(data_nreg$lag_log_cancer_costs_100k))
data_nreg <- data_nreg %>% mutate(d_lag_cancer_costs_100k = diff(data_nreg$lag_cancer_costs_100k))


#====================================================================================================
#                               Save data
#====================================================================================================


# Remove variables not needed for regression analysis
data_nreg <- data_nreg %>% 
  select(ID, year, gender, agegroup, nielsenreg, population,
         d_log_CVD_mort_100k, d_CVD_mort_100k,
         d_log_CVD_costs_100k, d_CVD_costs_100k,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k,
         d_log_cancer_costs_100k, d_cancer_costs_100k,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k) 

save(data_nreg, file = "./Data_ready/regression_nreg_level_three_period.rda")




#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on nieslsen region level but only two time periods                                    
#
# Input:    data_long
# Output:   regression_nreg_level_two_period.rda
#
#====================================================================================================
#====================================================================================================


#====================================================================================================
#                 Reshape to wide and then calculate differences between 2008 and 2017
#                 and their corresponding lags (2007 and 2016)
#====================================================================================================


# Reshape data to wide format
data_wide <- data_long %>% 
  subset(select = c(ID, year, nielsenreg, gender,agegroup, population,
                    log_CVD_mort_100k, log_CVD_costs_100k, CVD_mort_100k, CVD_costs_100k,
                    log_cancer_mort_100k, log_cancer_costs_100k, cancer_mort_100k, cancer_costs_100k))

data_wide <- data_wide %>%
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
#                                 Generate IDs on age/gender level
#====================================================================================================


# Generate ID for gender x age 
data_wide <- data_wide %>%
  group_by(gender, agegroup) %>%
  mutate(ID = cur_group_id())

# ID as factor variable
data_wide <- data_wide %>%
  mutate(ID = as.factor(ID))


#====================================================================================================
#                               Save data
#====================================================================================================


# Remove variables not needed for regression analysis, using population distribution at midpoint of 
# 2007 to 2017 for weighting regression

data_wide <- data_wide %>% 
  select(ID, nielsenreg, gender, agegroup, population_y2012,
         d_log_CVD_mort_100k, d_CVD_mort_100k,
         d_log_CVD_costs_100k, d_CVD_costs_100k,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k,
         d_log_cancer_costs_100k, d_cancer_costs_100k,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k) 

save(data_wide, file = "./Data_ready/regression_nreg_level_two_period.rda")













