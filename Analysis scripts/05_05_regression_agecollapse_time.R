
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using all states and four periods with collapsed agegroups                                       
#
# Input:    regression_state_level_four_period.rda
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
load("./Data_ready/regression_state_level_four_period.rda")


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
#                               Subset data
#====================================================================================================

# Remove variables not needed for regression analysis
data_agecollapse <- data_agecollapse %>% 
  select(ID, year, gender, agegroup, region, population,
         d_log_CVD_mort_100k, d_CVD_mort_100k, CVD_mort_100k, CVD_deaths,
         d_log_CVD_costs_100k, d_CVD_costs_100k, CVD_costs_100k, CVD_costs_sum,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k, cancer_deaths,
         d_log_cancer_costs_100k, d_cancer_costs_100k, cancer_costs_sum,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k)


#====================================================================================================
#                                       Conditioning on 60+
#====================================================================================================

data <- data_agecollapse

# Drop below 60, low variation in agegroups before (similar to van Baal et al., 2018), lot of noise
data <- data %>%
  filter(agegroup =="6" | agegroup == "7" | agegroup =="8" | agegroup =="9") %>% 
  filter(!(agegroup == "9" & gender =="Male"))

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
  filter(d_log_CVD_mort_100k != is.na) %>%
  ggplot(data = data, mapping = aes(x = d_log_CVD_mort_100k)) +
  geom_histogram() +
  stat_central_tendency(type = "median", linetype = "dashed")

hist4 <- data %>% 
  filter(d_log_CVD_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_log_CVD_costs_100k)) +
  geom_histogram() +
  stat_central_tendency(type = "median", linetype = "dashed")

scatter1 <- data %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k, y = d_CVD_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

scatter2 <- data %>%
  ggplot(data = data, mapping = aes(x = d_log_CVD_costs_100k, y = d_log_CVD_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")


# Combine plot
grid.arrange(hist1, hist3, hist2, hist4, scatter1, scatter2)


#====================================================================================================
#                                       Plotting temporal variation
#====================================================================================================


#====================================================================================================
#                                       Rescaling mortality to 100% - lags
#====================================================================================================


class(data)

data <- pdata.frame(data, index = c("ID", "year"))

# Graph with base 2008
data <- data %>%
  mutate(CVD_mort_100k_base2008 = 0)

data <- data %>% 
  mutate(CVD_mort_100k_base2008 = if_else(year == 1, 1, CVD_mort_100k_base2008),
         CVD_mort_100k_base2008 = if_else(year == 2, CVD_mort_100k/(lag(CVD_mort_100k)), CVD_mort_100k_base2008),
         CVD_mort_100k_base2008 = if_else(year == 3, CVD_mort_100k/(lag(CVD_mort_100k, n = 2)), CVD_mort_100k_base2008),
         CVD_mort_100k_base2008 = if_else(year == 4, CVD_mort_100k/(lag(CVD_mort_100k, n = 3)), CVD_mort_100k_base2008)
         )

# Male
line_plot_base2008_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_base2008, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ region, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "") +
  labs(y = "%-Difference in CVD mortality compared to 2008") +
  geom_hline(yintercept = 1)

line_plot_base2008_male

# Female
line_plot_base2008_female <- data %>%
  filter(gender == "Female") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_base2008, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ region, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference in CVD mortality compared to 2008") +
  geom_hline(yintercept = 1)

line_plot_base2008_female



# Graph with percentage change compared to previous year
data <- data %>%
  mutate(CVD_mort_100k_percent = 0)

data <- data %>% 
  mutate(CVD_mort_100k_percent = if_else(year == 1, 0, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 3, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 4, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent)
         )

# Male
line_plot_percent_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_percent, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ region, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference compared to previous year") +
  geom_hline(yintercept = 0)

line_plot_percent_male

# Female
line_plot_percent_female <- data %>%
  filter(gender == "Female") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_percent, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ region, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference compared to previous year") +
  geom_hline(yintercept = 0)

line_plot_percent_female



#====================================================================================================
#                                               Final CVD FD model
#====================================================================================================

library(car)
CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + year + region + agegroup*gender, data = data)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC0")

par(mfrow=c(2,2))
plot(CVD_reg)
vif(CVD_reg)

log_CVD_reg <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + year + region , data = data)
summary(log_CVD_reg)

coeftest(log_CVD_reg, vcov. = vcovHC, type = "HC0")

par(mfrow=c(2,2))
plot(log_CVD_reg)
vif(log_CVD_reg)

#====================================================================================================
#                                               Final cancer FD model
#====================================================================================================

cancer_reg <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k, data = data)
summary(cancer_reg)

coeftest(cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(cancer_reg)
vif(cancer_reg)

log_cancer_reg <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k, data = data)
summary(log_cancer_reg)

coeftest(log_cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(log_cancer_reg)
vif(log_cancer_reg)








#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using nielsenreg and four periods with collapsed agegroups                                       
#
# Input:    regression_state_agecollapse.rda
# Output:   Table XX
#
#====================================================================================================
#====================================================================================================


# remove files from the R space
rm(list=ls( ))

load("./Data_ready/regression_nreg_level_four_period.rda")


#====================================================================================================
#                                       Rename agegroups for collapse 
#====================================================================================================

data <- data_nreg
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
  subset(select = c(year, nielsenreg, gender, agegroup, CVD_costs_sum, CVD_deaths, cancer_costs_sum, cancer_deaths, population))

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
  select(year, gender, nielsenreg, ends_with("CVD_costs_sum")) %>% 
  pivot_longer(ends_with("CVD_costs_sum"), names_to = "agegroup", values_to = "CVD_costs_sum") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, nielsenreg) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# cancer costs
cancer_costs <- data_wide %>%
  select(year, gender, nielsenreg, ends_with("cancer_costs_sum")) %>% 
  pivot_longer(ends_with("cancer_costs_sum"), names_to = "agegroup", values_to = "cancer_costs_sum") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, nielsenreg) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# CVD deaths
CVD_deaths <- data_wide %>%
  select(year, gender, nielsenreg, ends_with("CVD_deaths")) %>% 
  pivot_longer(ends_with("CVD_deaths"), names_to = "agegroup", values_to = "CVD_deaths") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, nielsenreg) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# cancer deaths
cancer_deaths <- data_wide %>%
  select(year, gender, nielsenreg, ends_with("cancer_deaths")) %>% 
  pivot_longer(ends_with("cancer_deaths"), names_to = "agegroup", values_to = "cancer_deaths") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, nielsenreg) %>%
  mutate(ID = cur_group_id()) %>% 
  mutate(ID = as.factor(ID))

# Population
population <- data_wide %>%
  select(year, gender, nielsenreg, ends_with("population")) %>% 
  pivot_longer(ends_with("population"), names_to = "agegroup", values_to = "population") %>% 
  mutate(agegroup = substr(agegroup, start = 4, stop = 4)) %>% 
  group_by(gender, agegroup, nielsenreg) %>%
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
#                               Subset data
#====================================================================================================

# Save for nielsen nielsenreg transformation below
data_nreg <- data_agecollapse

# Remove variables not needed for regression analysis
data_agecollapse <- data_agecollapse %>% 
  select(ID, year, gender, agegroup, nielsenreg, population,
         d_log_CVD_mort_100k, d_CVD_mort_100k, CVD_mort_100k, CVD_deaths,
         d_log_CVD_costs_100k, d_CVD_costs_100k, CVD_costs_100k, CVD_costs_sum,
         d_lag_log_CVD_costs_100k, d_lag_CVD_costs_100k,
         d_log_cancer_mort_100k, d_cancer_mort_100k, cancer_deaths,
         d_log_cancer_costs_100k, d_cancer_costs_100k, cancer_costs_sum,
         d_lag_log_cancer_costs_100k, d_lag_cancer_costs_100k)


#====================================================================================================
#                                       Conditioning on 60+
#====================================================================================================

data <- data_agecollapse

# Drop below 60, low variation in agegroups before (similar to van Baal et al., 2018), lot of noise
data <- data %>%
  filter(agegroup =="6" | agegroup == "7" | agegroup =="8" | agegroup =="9") %>% 
  filter(!(agegroup == "9" & gender =="Male"))

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
  filter(d_log_CVD_mort_100k != is.na) %>%
  ggplot(data = data, mapping = aes(x = d_log_CVD_mort_100k)) +
  geom_histogram() +
  stat_central_tendency(type = "median", linetype = "dashed")

hist4 <- data %>% 
  filter(d_log_CVD_costs_100k !=0) %>%
  ggplot(data = data, mapping = aes(x = d_log_CVD_costs_100k)) +
  geom_histogram() +
  stat_central_tendency(type = "median", linetype = "dashed")

scatter1 <- data %>%
  ggplot(data = data, mapping = aes(x = d_CVD_costs_100k, y = d_CVD_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")

scatter2 <- data %>%
  ggplot(data = data, mapping = aes(x = d_log_CVD_costs_100k, y = d_log_CVD_mort_100k)) +
  geom_point() +
  geom_smooth(method = "lm")


# Combine plot
grid.arrange(hist1, hist3, hist2, hist4, scatter1, scatter2)


#====================================================================================================
#                                       Plotting temporal variation
#====================================================================================================


#====================================================================================================
#                                       Rescaling mortality to 100% - lags
#====================================================================================================


class(data)

data <- pdata.frame(data, index = c("ID", "year"))

# Graph with base 2008
data <- data %>%
  mutate(CVD_mort_100k_base2008 = 0)

data <- data %>% 
  mutate(CVD_mort_100k_base2008 = if_else(year == 1, 1, CVD_mort_100k_base2008),
         CVD_mort_100k_base2008 = if_else(year == 2, CVD_mort_100k/(lag(CVD_mort_100k)), CVD_mort_100k_base2008),
         CVD_mort_100k_base2008 = if_else(year == 3, CVD_mort_100k/(lag(CVD_mort_100k, n = 2)), CVD_mort_100k_base2008),
         CVD_mort_100k_base2008 = if_else(year == 4, CVD_mort_100k/(lag(CVD_mort_100k, n = 3)), CVD_mort_100k_base2008)
  )

# Male
line_plot_base2008_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_base2008, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ nielsenreg, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "") +
  labs(y = "%-Difference in CVD mortality compared to 2008") +
  geom_hline(yintercept = 1)

line_plot_base2008_male

# Female
line_plot_base2008_female <- data %>%
  filter(gender == "Female") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_base2008, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ nielsenreg, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference in CVD mortality compared to 2008") +
  geom_hline(yintercept = 1)

line_plot_base2008_female



# Graph with percentage change compared to previous year
data <- data %>%
  mutate(CVD_mort_100k_percent = 0)

data <- data %>% 
  mutate(CVD_mort_100k_percent = if_else(year == 1, 0, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 3, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 4, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent)
  )

# Male
line_plot_percent_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_percent, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ nielsenreg, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference compared to previous year") +
  geom_hline(yintercept = 0)

line_plot_percent_male

# Female
line_plot_percent_female <- data %>%
  filter(gender == "Female") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_percent, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ nielsenreg, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference compared to previous year") +
  geom_hline(yintercept = 0)

line_plot_percent_female



#====================================================================================================
#                                               Final CVD FD model
#====================================================================================================
library(car)
CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k, data = data)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC0")

par(mfrow=c(2,2))
plot(CVD_reg)
vif(CVD_reg)

log_CVD_reg <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k, data = data)
summary(log_CVD_reg)

coeftest(log_CVD_reg, vcov. = vcovHC, type = "HC0")

par(mfrow=c(2,2))
plot(log_CVD_reg)
vif(log_CVD_reg)

#====================================================================================================
#                                               Final cancer FD model
#====================================================================================================

cancer_reg <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k, data = data)
summary(cancer_reg)

coeftest(cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(cancer_reg)
vif(cancer_reg)

log_cancer_reg <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k, data = data)
summary(log_cancer_reg)

coeftest(log_cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(log_cancer_reg)
vif(log_cancer_reg)



