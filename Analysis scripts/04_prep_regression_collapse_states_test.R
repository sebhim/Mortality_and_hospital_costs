
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
library(viridis)
library(grid)
library(patchwork)
# Packages for maps and spatial data
library(rgdal)
library(spdep)
library(rgeos)
library(maptools)
library(sf)

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

# Drop just males 90+
# data <- data %>% filter(!(agegroup =="90+" & gender =="Male")) 


data <- data %>% filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+") 



#====================================================================================================
#                                       Reshape for collapsing data 
#====================================================================================================

# Reshape data to wide format

data_wide <- data %>% 
  subset(select = c(year, region, gender,agegroup, CVD_costs, CVD_deaths, cancer_costs, cancer_deaths, CVD_N, cancer_N, population))


data_wide <- data_wide %>% 
  pivot_wider(names_from = region, values_from = c(CVD_costs, CVD_deaths, cancer_costs, cancer_deaths, CVD_N, cancer_N, population), names_prefix = "bula_")


# Change censored cancer cost and cases which were censored due to low number of observations to value 
# in following year (2 instances)
# Otherwise complete observation would be lost

data_wide <- data_wide %>% 
  mutate(cancer_costs_bula_SL = replace_na(cancer_costs_bula_SL, cancer_costs_bula_SL[26])) %>% 
  mutate(cancer_N_bula_SL = replace_na(cancer_N_bula_SL, cancer_N_bula_SL[26])) %>% 
  mutate(cancer_costs_bula_HB = replace_na(cancer_costs_bula_HB, cancer_costs_bula_HB[143])) %>% 
  mutate(cancer_N_bula_HB = replace_na(cancer_N_bula_HB, cancer_N_bula_HB[143]))

# Combine State costs

# CVD
data_wide <- data_wide %>% 
  mutate(tot_CVD_costs = (CVD_costs_bula_SH*CVD_N_bula_SH +
                        CVD_costs_bula_HH*CVD_N_bula_HH +
                        CVD_costs_bula_NI*CVD_N_bula_NI +
                        CVD_costs_bula_HB*CVD_N_bula_HB +
                        CVD_costs_bula_NW*CVD_N_bula_NW +
                        CVD_costs_bula_HE*CVD_N_bula_HE +
                        CVD_costs_bula_RP*CVD_N_bula_RP +
                        CVD_costs_bula_BW*CVD_N_bula_BW +
                        CVD_costs_bula_BY*CVD_N_bula_BY +
                        CVD_costs_bula_SL*CVD_N_bula_SL +
                        CVD_costs_bula_BE*CVD_N_bula_BE +
                        CVD_costs_bula_BB*CVD_N_bula_BB +
                        CVD_costs_bula_MV*CVD_N_bula_MV +
                        CVD_costs_bula_SN*CVD_N_bula_SN +
                        CVD_costs_bula_ST*CVD_N_bula_ST +
                        CVD_costs_bula_TH*CVD_N_bula_TH))

# Cancer
data_wide <- data_wide %>% 
  mutate(tot_cancer_costs = (cancer_costs_bula_SH*cancer_N_bula_SH +
                        cancer_costs_bula_HH*cancer_N_bula_HH +
                        cancer_costs_bula_NI*cancer_N_bula_NI +
                        cancer_costs_bula_HB*cancer_N_bula_HB +
                        cancer_costs_bula_NW*cancer_N_bula_NW +
                        cancer_costs_bula_HE*cancer_N_bula_HE +
                        cancer_costs_bula_RP*cancer_N_bula_RP +
                        cancer_costs_bula_BW*cancer_N_bula_BW +
                        cancer_costs_bula_BY*cancer_N_bula_BY +
                        cancer_costs_bula_SL*cancer_N_bula_SL +
                        cancer_costs_bula_BE*cancer_N_bula_BE +
                        cancer_costs_bula_BB*cancer_N_bula_BB +
                        cancer_costs_bula_MV*cancer_N_bula_MV +
                        cancer_costs_bula_SN*cancer_N_bula_SN +
                        cancer_costs_bula_ST*cancer_N_bula_ST +
                        cancer_costs_bula_TH*cancer_N_bula_TH))

# Combine State mortality

# CVD
data_wide <- data_wide %>% 
  mutate(tot_CVD_deaths = (CVD_deaths_bula_SH +
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
  mutate(tot_cancer_deaths = (cancer_deaths_bula_SH +
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
  mutate(tot_population = (population_bula_SH +
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
  select(year, gender, agegroup, tot_CVD_costs, tot_cancer_costs, tot_CVD_deaths, tot_cancer_deaths, tot_population)


#====================================================================================================
#                                       Generate IDs
#====================================================================================================


# Generate ID for gender x age  groups
data_wide <- data_wide %>% group_by(gender, agegroup) %>% mutate(ID = cur_group_id())

# summarize the variables
summary(data_wide[, c("ID", "year")])

class(data_wide$ID)
class(data_wide$year)

# Recode ID to be factor
data_wide <- data_wide %>% mutate(ID = as.factor(ID))
class(data_wide$ID)


#====================================================================================================
#                               Generate mortality and costs per capita
#====================================================================================================

# Prepare variables
data_wide <- data_wide %>% 
  mutate(CVD_mort = tot_CVD_deaths/tot_population) %>%
  mutate(cancer_mort = tot_cancer_deaths/tot_population) %>%
  mutate(CVD_costs = tot_CVD_costs/tot_population) %>%
  mutate(cancer_costs = tot_cancer_costs/tot_population) %>%
  mutate(log_CVD_mort = log(CVD_mort)) %>%
  mutate(log_cancer_mort = log(cancer_mort)) %>%
  mutate(log_CVD_costs = log(CVD_costs)) %>%
  mutate(log_cancer_costs = log(cancer_costs))
  


# #====================================================================================================
# #                           Prepare mortality data for CVD and cancer
# #====================================================================================================
# 
# 
# # Keep observations with 0 mortality by adding constant 0.00001 and drop columns not needed
# data <- data %>%
#   mutate(log_CVD_mort_100k = recode(log_CVD_mort_100k, "-Inf" = log(10^-5))) %>%
#   mutate(log_cancer_mort_100k = recode(log_cancer_mort_100k, "-Inf" = log(10^-5))) %>% 
#   select(ID, year, log_CVD_mort_100k, CVD_mort_100k, log_CVD_costs_100k, CVD_costs_100k, log_cancer_mort_100k, cancer_mort_100k, log_cancer_costs_100k, cancer_costs_100k) 
# 
# # Drop NAs, 2 observations dropped as cancer cost was not available, censored by DRG data holder
# # due to number of cases below 5
# data <- data %>%  filter(!is.na(log_CVD_costs_100k))
# data <- data %>%  filter(!is.na(log_cancer_costs_100k))


#====================================================================================================
#                       Create lagged values and individual and time dummies
#====================================================================================================


# Setting dataset to be panel 
class(data_wide)
data_wide <- pdata.frame(data_wide, index = c("ID", "year"))

# xtsum panel summary
is.pbalanced(data_wide)
pdim(data_wide)

# Generate lagged cost variables
data_wide <- data_wide %>%
  group_by(ID) %>%
  mutate(lag_log_CVD_costs=dplyr::lag(log_CVD_costs)) %>% 
  mutate(lag_log_cancer_costs=dplyr::lag(log_cancer_costs)) %>% 
  mutate(lag_CVD_costs=dplyr::lag(CVD_costs)) %>% 
  mutate(lag_cancer_costs=dplyr::lag(cancer_costs))

# Generate ID dummies
model.matrix( ~ ID - 1, data_wide)
data_wide <- data.frame(data_wide, model.matrix( ~ ID - 1, data_wide))


# Generate year dummies
model.matrix( ~ year - 1, data_wide)
data_wide <- data.frame(data_wide, model.matrix( ~ year - 1, data_wide))


#====================================================================================================
#                             Manually creating first differences
#====================================================================================================


# Panel set data
data_wide <- pdata.frame(data_wide, index = c("ID", "year"))


# Mortality CVD
data_wide <- data_wide %>% mutate(d_log_CVD_mort = diff(data_wide$log_CVD_mort))
data_wide <- data_wide %>% mutate(d_CVD_mort = diff(data_wide$CVD_mort))

# Costs CVD  
data_wide <- data_wide %>% mutate(d_log_CVD_costs = diff(data_wide$log_CVD_costs))
data_wide <- data_wide %>% mutate(d_CVD_costs = diff(data_wide$CVD_costs))

# Lag costs CVD
data_wide <- data_wide %>% mutate(d_lag_log_CVD_costs = diff(data_wide$lag_log_CVD_costs))
data_wide <- data_wide %>% mutate(d_lag_CVD_costs = diff(data_wide$lag_CVD_costs))


# Mortality cancer
data_wide <- data_wide %>% mutate(d_log_cancer_mort = diff(data_wide$log_cancer_mort))
data_wide <- data_wide %>% mutate(d_cancer_mort = diff(data_wide$cancer_mort))

# Costs cancer  
data_wide <- data_wide %>% mutate(d_log_cancer_costs = diff(data_wide$log_cancer_costs))
data_wide <- data_wide %>% mutate(d_cancer_costs = diff(data_wide$cancer_costs))

# Lag costs cancer
data_wide <- data_wide %>% mutate(d_lag_log_cancer_costs = diff(data_wide$lag_log_cancer_costs))
data_wide <- data_wide %>% mutate(d_lag_cancer_costs = diff(data_wide$lag_cancer_costs))


#====================================================================================================
#                             Run regression using CVD mortality and costs
#====================================================================================================


# Filter (region quite different, but all positive first coefficient, NW lagged negative slightly larger
# Also tested if the same when not logging but effect persists
# Testing if the same when just comparing two years 2009 and 2017, 2007 and 2008 needed for creating 
# lagged variables and first differences, still one coefficient positive the other negative, with
# the positive one being larger.
data_agg_graph <- data_wide

data_wide <- data_wide %>% 
  filter(!(agegroup == "90+" & gender =="Male"))

# OLS model 
m1_OLS <- lm(log_CVD_mort ~ log_CVD_costs + lag_log_CVD_costs + year + ID , data = data_wide)
m1_OLS

m1_OLS_lin <- lm(CVD_mort ~ CVD_costs + lag_CVD_costs + year + ID , data = data_wide)
m1_OLS_lin


# # Subset data and run with all variables
# CVD_fd_data <- data %>% 
#   select(d_log_CVD_mort, d_log_CVD_costs, d_lag_log_CVD_costs, ID1:ID208, year2007:year2017)
# 
# # First difference model on subsetted data
# lm(d_log_CVD_mort ~ . , data = CVD_fd_data)

# First difference model on full data using year and ID as factors
# same output, different reference categories for year and ID
m2_FD <- lm(d_log_CVD_mort ~ d_log_CVD_costs + d_lag_log_CVD_costs + year + ID , data = data_wide)
summ(m2_FD)

# This would correspond to a replication of van Baal et al. (2018)
# On country level, with ageXgender cells as intercepts and year intercepts

m2_FD_lin <- lm(d_CVD_mort ~ d_CVD_costs + d_lag_CVD_costs + year + ID, data = data_wide)
m2_FD_lin


coeftest(m1_OLS, vcov = vcovHC(m1_OLS, type="HC0", cluster="ID"))
coeftest(m2_FD, vcov = vcovHC(m2_FD, type="HC0", cluster="ID"))

summ(m2_FD)
summ(m1_OLS)


#====================================================================================================
#                             Run regression using cancer mortality and costs
#====================================================================================================


# Filter (region quite different, but all positive first coefficient, NW lagged negative slightly larger
# Also tested if the same when not logging but effect persists
# Testing if the same when just comparing two years 2009 and 2017, 2007 and 2008 needed for creating 
# lagged variables and first differences, still one coefficient positive the other negative, with
# the positive one being larger.

#data_wide <- data_wide %>% 
#  filter(gender == "Male")

# OLS model 
m1_OLS <- lm(log_cancer_mort ~ log_cancer_costs + lag_log_cancer_costs + year + ID , data = data_wide)
m1_OLS

m1_OLS_lin <- lm(cancer_mort ~ cancer_costs + lag_cancer_costs + year + ID , data = data_wide)
m1_OLS_lin


# # Subset data and run with all variables
# cancer_fd_data <- data %>% 
#   select(d_log_cancer_mort, d_log_cancer_costs, d_lag_log_cancer_costs, ID1:ID208, year2007:year2017)
# 
# # First difference model on subsetted data
# lm(d_log_cancer_mort ~ . , data = cancer_fd_data)

# First difference model on full data using year and ID as factors
# same output, different reference categories for year and ID
m2_FD <- lm(d_log_cancer_mort ~ d_log_cancer_costs + d_lag_log_cancer_costs + year + ID, data = data_wide)
summ(m2_FD)

m2_FD_lin <- lm(d_cancer_mort ~ d_cancer_costs + d_lag_cancer_costs + year + ID, data = data_wide)
m2_FD_lin


coeftest(m1_OLS, vcov = vcovHC(m1_OLS, type="HC0", cluster="ID"))
coeftest(m2_FD, vcov = vcovHC(m2_FD, type="HC0", cluster="ID"))

summ(m2_FD)
summ(m1_OLS)



#====================================================================================================
#                        Plotting mort diff vs 
#====================================================================================================

data_wide %>% 
  ggplot()+
  geom_point(aes(x= cancer_costs, y = cancer_mort, colour = agegroup, shape = gender))

data_wide %>% 
  ggplot()+
  geom_point(aes(x= CVD_costs, y = CVD_mort, colour = agegroup, shape = gender))



#====================================================================================================
#                             Plot differences over time in mort CVD
#====================================================================================================


data_wide <- data_agg_graph

# Male
m_CVD_mort <- data_wide %>% 
  filter(gender == "Male", year != "2007") %>%
  ggplot(aes(x = agegroup, y = reorder(year, desc(year)))) +
  geom_tile(aes(fill = d_CVD_mort)) +
  scale_fill_viridis(limits=c(-0.011, 0), breaks=seq(-0.010,0,by=0.005)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8)) +
  labs(fill = "Change in mortality per capita") +
  ggtitle("Male")+
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))

# Female
f_CVD_mort <-data_wide %>% 
  filter(gender == "Female", year != "2007") %>%
  ggplot(aes(x = agegroup, y = reorder(year, desc(year)))) +
  geom_tile(aes(fill = d_CVD_mort)) +
  scale_fill_viridis(limits=c(-0.011, 0), breaks=seq(-0.010,0,by=0.005)) +
  theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8))+
  labs(fill = "Change in mortality per capita") +
  ggtitle("Female") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5))


#====================================================================================================
#                             Plot differences over time in costs CVD
#====================================================================================================


# Male
m_CVD_costs <- data_wide %>% 
  filter(gender == "Male", year != "2007") %>%
  ggplot(aes(x = agegroup, y = reorder(year, desc(year)))) +
  geom_tile(aes(fill = d_CVD_costs)) +
  scale_fill_viridis(option = "magma", direction = -1, limits=c(0, 75), breaks=seq(0,75,by=25)) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8)) +
  labs(fill = "Change in costs per capita") +
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))

# Female
f_CVD_costs <-data_wide %>% 
  filter(gender == "Female", year != "2007") %>%
  ggplot(aes(x = agegroup, y = reorder(year, desc(year)))) +
  geom_tile(aes(fill = d_CVD_costs)) +
  scale_fill_viridis(option = "magma", direction = -1, limits=c(0, 75), breaks=seq(0,75,by=25)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8))+
  labs(fill = "Change in costs per capita") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5))

# Combine graph


m_CVD_mort  + f_CVD_mort  + m_CVD_costs + f_CVD_costs +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")








#====================================================================================================
#                             Plot differences over time in mort cancer
#====================================================================================================


data_wide <- data_agg_graph

# Male
m_cancer_mort <- data_wide %>% 
  filter(gender == "Male", year != "2007") %>%
  ggplot(aes(x = agegroup, y = reorder(year, desc(year)))) +
  geom_tile(aes(fill = d_cancer_mort)) +
  scale_fill_viridis(limits=c(-0.002, 0), breaks=seq(-0.002,0,by=0.001)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8)) +
  labs(fill = "Change in mortality per capita") +
  ggtitle("Male")+
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))

# Female
f_cancer_mort <-data_wide %>% 
  filter(gender == "Female", year != "2007") %>%
  ggplot(aes(x = agegroup, y = reorder(year, desc(year)))) +
  geom_tile(aes(fill = d_cancer_mort)) +
  scale_fill_viridis(limits=c(-0.002, 0), breaks=seq(-0.002,0,by=0.001)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8))+
  labs(fill = "Change in mortality per capita") +
  ggtitle("Female") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5))


#====================================================================================================
#                             Plot differences over time in costs cancer
#====================================================================================================


# Male
m_cancer_costs <- data_wide %>% 
  filter(gender == "Male", year != "2007") %>%
  ggplot(aes(x = agegroup, y = reorder(year, desc(year)))) +
  geom_tile(aes(fill = d_cancer_costs)) +
  scale_fill_viridis(option = "magma", direction = -1, limits=c(0, 75), breaks=seq(0,75,by=25)) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8)) +
  labs(fill = "Change in costs per capita") +
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))

# Female
f_cancer_costs <-data_wide %>% 
  filter(gender == "Female", year != "2007") %>%
  ggplot(aes(x = agegroup, y = reorder(year, desc(year)))) +
  geom_tile(aes(fill = d_cancer_costs)) +
  scale_fill_viridis(option = "magma", direction = -1, limits=c(0, 75), breaks=seq(0,75,by=25)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8))+
  labs(fill = "Change in costs per capita") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5))

# Combine graph


m_cancer_mort  + f_cancer_mort  + m_cancer_costs + f_cancer_costs +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
















