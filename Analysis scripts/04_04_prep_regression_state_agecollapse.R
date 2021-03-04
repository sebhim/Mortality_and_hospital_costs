
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on state level collapsing age categories
#           to reduce noise in the data 
#
# Input:    clean_data.rda
# Output:   regression_state_level_agecollapse.rda
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
#                                       Rename agegroups for collapse 
#====================================================================================================


data <- data %>%
  mutate(agegroup = recode(agegroup, "<1" = "1",
                                    "1-14" = "2",
                                    "15-19" = "3",
                                    "20-24" = "4",
                                    "25-29" = "5",
                                    "30-34" = "6",
                                    "35-39" = "7",
                                    "40-44" = "8",
                                    "45-49" = "9",
                                    "50-54" = "10",
                                    "55-59" = "11",
                                    "60-64" = "12",
                                    "65-69" = "13",
                                    "70-74" = "14",
                                    "75-79" = "15",
                                    "80-84" = "16",
                                    "85-89" = "17",
                                    "90+" = "18"))


#====================================================================================================
#                                       Reshape for collapsing data 
#====================================================================================================


# Subset data to variables needed
data <- data %>% 
  subset(select = c(year, region, gender, agegroup, CVD_costs_sum, CVD_deaths, cancer_costs_sum, cancer_deaths, population))

# Reshape data to wide format
data_wide <- data %>% 
  pivot_wider(names_from = agegroup, values_from = c(CVD_costs_sum, CVD_deaths, cancer_costs_sum, cancer_deaths, population), names_prefix = "agegroup_")

# Change censored cancer cost and cases which were censored due to number of cases < 2 to 0 
data_wide <- data_wide %>% 
    mutate(across(contains("_sum_"), ~replace_na(.x, 0)))


#====================================================================================================
#                           Combine costs to agegroups
#====================================================================================================


# CVD
data_wide <- data_wide %>% 
  mutate(age1_CVD_costs_sum = (CVD_costs_sum_agegroup_1 + CVD_costs_sum_agegroup_2 + CVD_costs_sum_agegroup_3)) %>% 
  mutate(age2_CVD_costs_sum = (CVD_costs_sum_agegroup_4 + CVD_costs_sum_agegroup_5)) %>% 
  mutate(age3_CVD_costs_sum = (CVD_costs_sum_agegroup_6 + CVD_costs_sum_agegroup_7)) %>% 
  mutate(age4_CVD_costs_sum = (CVD_costs_sum_agegroup_8 + CVD_costs_sum_agegroup_9)) %>% 
  mutate(age5_CVD_costs_sum = (CVD_costs_sum_agegroup_10 + CVD_costs_sum_agegroup_11)) %>% 
  mutate(age6_CVD_costs_sum = (CVD_costs_sum_agegroup_12 + CVD_costs_sum_agegroup_13)) %>% 
  mutate(age7_CVD_costs_sum = (CVD_costs_sum_agegroup_14 + CVD_costs_sum_agegroup_15)) %>% 
  mutate(age8_CVD_costs_sum = (CVD_costs_sum_agegroup_16 + CVD_costs_sum_agegroup_17)) %>% 
  mutate(age9_CVD_costs_sum = (CVD_costs_sum_agegroup_18))
  
  
# cancer
data_wide <- data_wide %>% 
  mutate(age1_cancer_costs_sum = (cancer_costs_sum_agegroup_1 + cancer_costs_sum_agegroup_2 + cancer_costs_sum_agegroup_3)) %>% 
  mutate(age2_cancer_costs_sum = (cancer_costs_sum_agegroup_4 + cancer_costs_sum_agegroup_5)) %>% 
  mutate(age3_cancer_costs_sum = (cancer_costs_sum_agegroup_6 + cancer_costs_sum_agegroup_7)) %>% 
  mutate(age4_cancer_costs_sum = (cancer_costs_sum_agegroup_8 + cancer_costs_sum_agegroup_9)) %>% 
  mutate(age5_cancer_costs_sum = (cancer_costs_sum_agegroup_10 + cancer_costs_sum_agegroup_11)) %>% 
  mutate(age6_cancer_costs_sum = (cancer_costs_sum_agegroup_12 + cancer_costs_sum_agegroup_13)) %>% 
  mutate(age7_cancer_costs_sum = (cancer_costs_sum_agegroup_14 + cancer_costs_sum_agegroup_15)) %>% 
  mutate(age8_cancer_costs_sum = (cancer_costs_sum_agegroup_16 + cancer_costs_sum_agegroup_17)) %>% 
  mutate(age9_cancer_costs_sum = (cancer_costs_sum_agegroup_18))


#====================================================================================================
#                           Combine mortality to agegroups
#====================================================================================================


# CVD
data_wide <- data_wide %>% 
  mutate(age1_CVD_deaths = (CVD_deaths_agegroup_1 + CVD_deaths_agegroup_2 + CVD_deaths_agegroup_3)) %>% 
  mutate(age2_CVD_deaths = (CVD_deaths_agegroup_4 + CVD_deaths_agegroup_5)) %>% 
  mutate(age3_CVD_deaths = (CVD_deaths_agegroup_6 + CVD_deaths_agegroup_7)) %>% 
  mutate(age4_CVD_deaths = (CVD_deaths_agegroup_8 + CVD_deaths_agegroup_9)) %>% 
  mutate(age5_CVD_deaths = (CVD_deaths_agegroup_10 + CVD_deaths_agegroup_11)) %>% 
  mutate(age6_CVD_deaths = (CVD_deaths_agegroup_12 + CVD_deaths_agegroup_13)) %>% 
  mutate(age7_CVD_deaths = (CVD_deaths_agegroup_14 + CVD_deaths_agegroup_15)) %>% 
  mutate(age8_CVD_deaths = (CVD_deaths_agegroup_16 + CVD_deaths_agegroup_17)) %>% 
  mutate(age9_CVD_deaths = (CVD_deaths_agegroup_18))

# Cancer
data_wide <- data_wide %>% 
  mutate(age1_cancer_deaths = (cancer_deaths_agegroup_1 + cancer_deaths_agegroup_2 + cancer_deaths_agegroup_3)) %>% 
  mutate(age2_cancer_deaths = (cancer_deaths_agegroup_4 + cancer_deaths_agegroup_5)) %>% 
  mutate(age3_cancer_deaths = (cancer_deaths_agegroup_6 + cancer_deaths_agegroup_7)) %>% 
  mutate(age4_cancer_deaths = (cancer_deaths_agegroup_8 + cancer_deaths_agegroup_9)) %>% 
  mutate(age5_cancer_deaths = (cancer_deaths_agegroup_10 + cancer_deaths_agegroup_11)) %>% 
  mutate(age6_cancer_deaths = (cancer_deaths_agegroup_12 + cancer_deaths_agegroup_13)) %>% 
  mutate(age7_cancer_deaths = (cancer_deaths_agegroup_14 + cancer_deaths_agegroup_15)) %>% 
  mutate(age8_cancer_deaths = (cancer_deaths_agegroup_16 + cancer_deaths_agegroup_17)) %>% 
  mutate(age9_cancer_deaths = (cancer_deaths_agegroup_18))


#====================================================================================================
#                           Combine population to age groups
#====================================================================================================


data_wide <- data_wide %>% 
  mutate(age1_population = (population_agegroup_1 + population_agegroup_2 + population_agegroup_3)) %>% 
  mutate(age2_population = (population_agegroup_4 + population_agegroup_5)) %>% 
  mutate(age3_population = (population_agegroup_6 + population_agegroup_7)) %>% 
  mutate(age4_population = (population_agegroup_8 + population_agegroup_9)) %>% 
  mutate(age5_population = (population_agegroup_10 + population_agegroup_11)) %>% 
  mutate(age6_population = (population_agegroup_12 + population_agegroup_13)) %>% 
  mutate(age7_population = (population_agegroup_14 + population_agegroup_15)) %>% 
  mutate(age8_population = (population_agegroup_16 + population_agegroup_17)) %>% 
  mutate(age9_population = (population_agegroup_18))


#====================================================================================================
#           Generate long reshaped vectors of costs, mortality and population
#====================================================================================================


# CVD costs
CVD_costs <- data_wide %>%
  select(year, gender, region, ends_with("CVD_costs_sum")) %>% 
  pivot_longer(ends_with("CVD_costs_sum"), names_to = "agegroup", values_to = "CVD_costs_sum") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, region) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# cancer costs
cancer_costs <- data_wide %>%
  select(year, gender, region, ends_with("cancer_costs_sum")) %>% 
  pivot_longer(ends_with("cancer_costs_sum"), names_to = "agegroup", values_to = "cancer_costs_sum") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, region) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# CVD deaths
CVD_deaths <- data_wide %>%
  select(year, gender, region, ends_with("CVD_deaths")) %>% 
  pivot_longer(ends_with("CVD_deaths"), names_to = "agegroup", values_to = "CVD_deaths") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, region) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# cancer deaths
cancer_deaths <- data_wide %>%
  select(year, gender, region, ends_with("cancer_deaths")) %>% 
  pivot_longer(ends_with("cancer_deaths"), names_to = "agegroup", values_to = "cancer_deaths") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, region) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# Population
population <- data_wide %>%
  select(year, gender, region, ends_with("population")) %>% 
  pivot_longer(ends_with("population"), names_to = "agegroup", values_to = "population") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, region) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))


#====================================================================================================
#                 Merge reshaped vectors of costs, mortality and population
#====================================================================================================


data_agecollapse <- inner_join(cancer_costs, cancer_deaths, by= NULL) %>% 
  inner_join(., CVD_costs, by= NULL) %>% 
  inner_join(., CVD_deaths, by= NULL) %>% 
  inner_join(., population, by= NULL)


#====================================================================================================
#                               Generate mortality and costs per 100k population
#====================================================================================================


# Prepare variables
data_agecollapse <- data_agecollapse %>% 
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
data_agecollapse <- pdata.frame(data_agecollapse, index = c("ID", "year"))

# xtsum panel summary
is.pbalanced(data_agecollapse)
pdim(data_agecollapse)


#====================================================================================================
#                       Create lagged values and individual and time dummies
#====================================================================================================


# Generate lagged cost variables
data_agecollapse <- data_agecollapse %>%
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
data_long_agecollapse <- data_agecollapse

#====================================================================================================
#                             Manually creating first differences
#====================================================================================================


# Panel set data frame
data_agecollapse <- pdata.frame(data_agecollapse, index = c("ID", "year"))


# Mortality CVD
data_agecollapse <- data_agecollapse %>% mutate(d_log_CVD_mort_100k = diff(data_agecollapse$log_CVD_mort_100k))
data_agecollapse <- data_agecollapse %>% mutate(d_CVD_mort_100k = diff(data_agecollapse$CVD_mort_100k))

# Costs CVD  
data_agecollapse <- data_agecollapse %>% mutate(d_log_CVD_costs_100k = diff(data_agecollapse$log_CVD_costs_100k))
data_agecollapse <- data_agecollapse %>% mutate(d_CVD_costs_100k = diff(data_agecollapse$CVD_costs_100k))

# Lag costs CVD
data_agecollapse <- data_agecollapse %>% mutate(d_lag_log_CVD_costs_100k = diff(data_agecollapse$lag_log_CVD_costs_100k))
data_agecollapse <- data_agecollapse %>% mutate(d_lag_CVD_costs_100k = diff(data_agecollapse$lag_CVD_costs_100k))


# Mortality cancer
data_agecollapse <- data_agecollapse %>% mutate(d_log_cancer_mort_100k = diff(data_agecollapse$log_cancer_mort_100k))
data_agecollapse <- data_agecollapse %>% mutate(d_cancer_mort_100k = diff(data_agecollapse$cancer_mort_100k))

# Costs cancer  
data_agecollapse <- data_agecollapse %>% mutate(d_log_cancer_costs_100k = diff(data_agecollapse$log_cancer_costs_100k))
data_agecollapse <- data_agecollapse %>% mutate(d_cancer_costs_100k = diff(data_agecollapse$cancer_costs_100k))

# Lag costs cancer
data_agecollapse <- data_agecollapse %>% mutate(d_lag_log_cancer_costs_100k = diff(data_agecollapse$lag_log_cancer_costs_100k))
data_agecollapse <- data_agecollapse %>% mutate(d_lag_cancer_costs_100k = diff(data_agecollapse$lag_cancer_costs_100k))


#====================================================================================================
#                               Save data
#====================================================================================================

# Save for nielsen region transformation below
data_nreg <- data_agecollapse

# Remove variables not needed for regression analysis
data_agecollapse <- data_agecollapse %>% 
  select(ID, year, gender, agegroup, region, population,
         d_log_CVD_mort_100k, d_CVD_mort_100k, CVD_mort_100k, CVD_deaths,
         d_log_CVD_costs_100k, d_CVD_costs_100k, CVD_costs_100k, CVD_costs_sum,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k, cancer_deaths,
         d_log_cancer_costs_100k, d_cancer_costs_100k, cancer_costs_sum,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k) 

save(data_agecollapse, file = "./Data_ready/regression_state_agecollapse.rda")




#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Prepare data for regression analysis using data on Nielssen region level
#           and reduced age categories.
#
# Input:    data_agecollapse
# Output:   regression_nreg_agecollapse.rda
#
#====================================================================================================
#====================================================================================================


#====================================================================================================
#                                       Reshape for collapsing data 
#====================================================================================================


# Subset data to variables needed
data <- data_nreg %>% 
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

# revert to data frame for plotting
data <- as.data.frame(data)

# # Analysis why Log so different results
# data_agecollapse %>% 
#   filter(!(agegroup ==1 | agegroup ==2 | agegroup ==3 | agegroup ==4 | agegroup ==5)) %>% 
#   ggplot(aes(d_log_CVD_costs_100k, d_log_CVD_mort_100k)) + geom_point()  #+
#   #geom_smooth(method = "lm")
# 
# 
# 
# data_agecollapse %>%  ggplot(aes(d_CVD_mort_100k, d_CVD_costs_100k)) + geom_point()




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
         d_log_CVD_mort_100k, d_CVD_mort_100k, CVD_mort_100k,
         d_log_CVD_costs_100k, d_CVD_costs_100k, CVD_costs_100k,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k,
         d_log_cancer_costs_100k, d_cancer_costs_100k,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k) 

save(data_nreg, file = "./Data_ready/regression_nreg_agecollapse.rda")
