
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis                                          
#
# Input:    clean_data.rda
# Output:   Tables and Figures
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

load("./Data_ready/clean_data.rda")


#====================================================================================================
#                                       Conditioning on 60+, dropping males 90+
#====================================================================================================


is.data.frame(data)
dim(data)
str(data)
head(data)


# Drop below 60, low variation in agegroups before (similar to van Baal et al., 2018)
# Drop 90+ men before 2011 due to comparability issues (after 2011 based on widely different population values)
#data <- data %>% filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | (agegroup =="90+" & gender =="Female")) 

data <- data %>% filter(!(agegroup =="90+" & gender =="Male")) 

#====================================================================================================
#                                       Generate IDs
#====================================================================================================


# Generate ID for gender x age x state groups
data <- data %>% group_by(region, gender, agegroup) %>% mutate(ID = cur_group_id())

# summarize the variables 'state' and 'year'
summary(data[, c("ID", "year")])

class(data$ID)
class(data$year)

# Recode ID to be factor
data <- data %>% mutate(ID = as.factor(ID))
class(data$ID)


#====================================================================================================
#                           Prepare mortality data for CVD and cancer
#====================================================================================================


# Keep observations with 0 mortality by adding constant 0.00001 and drop columns not needed
data <- data %>%
  mutate(log_CVD_mort_100k = recode(log_CVD_mort_100k, "-Inf" = log(10^-5))) %>%
  mutate(log_cancer_mort_100k = recode(log_cancer_mort_100k, "-Inf" = log(10^-5))) %>% 
  select(ID, year, log_CVD_mort_100k, CVD_mort_100k, log_CVD_costs_100k, CVD_costs_100k, log_cancer_mort_100k, cancer_mort_100k, log_cancer_costs_100k, cancer_costs_100k, population) 

# Drop NAs, 2 observations dropped as cancer cost was not available, censored by DRG data holder
# due to number of cases below 5
data <- data %>%  filter(!is.na(log_CVD_costs_100k))
data <- data %>%  filter(!is.na(log_cancer_costs_100k))


#====================================================================================================
#                       Create lagged values and individual and time dummies
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
  mutate(lag_log_CVD_costs_100k=dplyr::lag(log_CVD_costs_100k)) %>% 
  mutate(lag_log_cancer_costs_100k=dplyr::lag(log_cancer_costs_100k)) %>% 
  mutate(lag_CVD_costs_100k=dplyr::lag(CVD_costs_100k)) %>% 
  mutate(lag_cancer_costs_100k=dplyr::lag(cancer_costs_100k))

# Generate ID dummies
model.matrix( ~ ID - 1, data)
data <- data.frame(data, model.matrix( ~ ID - 1, data))


# Generate year dummies
model.matrix( ~ year - 1, data)
data <- data.frame(data, model.matrix( ~ year - 1, data))


#====================================================================================================
#                             Manually creating first differences
#====================================================================================================

# IF only use two period model first full data 2009? 
data <- data %>% 
  filter(year == "2008" | year =="2017") %>% 
  mutate(year = replace(year, year =="2008", "2016"))

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
#                             Run regression using CVD mortality and costs
#====================================================================================================


# Filter (region quite different, but all positive first coefficient, NW lagged negative slightly larger
# Also tested if the same when not logging but effect persists
# Testing if the same when just comparing two years 2009 and 2017, 2007 and 2008 needed for creating 
# lagged variables and first differences, still one coefficient positive the other negative, with
# the positive one being larger.


#====================================================================================================
#                             Data conditioning and subgroups
#====================================================================================================


#data <- data %>% 
#  filter(agegroup == "70-74", region != "HH", region != "SL", region != "HB")


#data <- data %>% 
#  filter(agegroup == "70-74" & (region == "BW" | region == "NW" | region == "NI" | region == "HE"))

#data <- data %>% 
# filter(year ==2012 | year ==2013 | year ==2014 | year ==2015 | year ==2016 | year ==2017)

#data <- data %>% 
# filter(agegroup == "75-79" | agegroup == "80-84" | agegroup == "85-89" | agegroup == "90+")


#data <- data %>% 
#  filter(agegroup == "90+")

# data <- data %>% 
#   filter(year == "2009" | year =="2017")



# Setting weights
# https://stackoverflow.com/questions/18260017/how-to-set-a-weighted-least-squares-in-r-for-heteroscedastic-data

#plm fd
#FD_reg_CVD <- plm(log_CVD_mort_100k ~ log_CVD_costs_100k + lag_log_CVD_costs_100k, data = data, weights = 1/data$population, model = "fd")


cancer_FD <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k + ID, data = data, weights = 1/data$population)
summ(cancer_FD)


CVD_FD <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k, data = data, weights = 1/data$population)
summ(CVD_FD)

# With ID overspecified, control for state fixed effects and gender x age group fixed effects instead of ID?
# Generate ID for gender x age
data <- data %>% group_by(gender, agegroup) %>% mutate(ageXgender = cur_group_id())
class(data$ageXgender)

# Recode gender age cel to be factor
data <- data %>% mutate(ageXgender = as.factor(ageXgender))
class(data$ageXgender)
class(data$region)

CVD_FD <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k + ageXgender, data = data, weights = 1/data$population)
summ(CVD_FD)

cancer_FD <- lm(d_log_CVD_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k + ageXgender, data = data, weights = 1/data$population)
summ(cancer_FD)


# OLS model 
m1_OLS <- lm(log_CVD_mort_100k ~ log_CVD_costs_100k + lag_log_CVD_costs_100k + year + ID , data = data)
coeftest(m1_OLS)
stargazer(m1_OLS)


m1_OLS_lin <- lm(CVD_mort_100k ~ CVD_costs_100k + lag_CVD_costs_100k + year + ID , data = data)
m1_OLS_lin


# # Subset data and run with all variables
# CVD_fd_data <- data %>% 
#   select(d_log_CVD_mort_100k, d_log_CVD_costs_100k, d_lag_log_CVD_costs_100k, ID1:ID208, year2007:year2017)
# 
# # First difference model on subsetted data
# lm(d_log_CVD_mort_100k ~ . , data = CVD_fd_data)

# First difference model on full data using year and ID as factors
# same output, different reference categories for year and ID
m2_FD <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k + year + ID, data = data)
m2_FD

m2_FD_lin <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + ID, data = data)
m2_FD_lin



OLS_se <- coeftest(m1_OLS, vcov = vcovHC(m1_OLS, type="HC0", cluster="ID"))
FD_se <- coeftest(m2_FD, vcov = vcovHC(m2_FD, type="HC0", cluster="ID"))


stargazer(OLS_se, FD_se,
          type = "latex",
          title = "Regression results using states",
          omit = c("year", "ID"),
          digits = 3,
          header = FALSE,
          single.row = TRUE,
          column.labels = c("(I)", "(II)"))



#====================================================================================================
#                        Plotting mort diff vs for postive pater below 75
#====================================================================================================

data %>% 
  ggplot()+
  geom_point(aes(x= CVD_costs_100k, y = CVD_mort_100k, colour = region, shape = gender))

data %>% 
  ggplot()+
  geom_point(aes(x= cancer_costs_100k, y = cancer_mort_100k, colour = region, shape = gender))






