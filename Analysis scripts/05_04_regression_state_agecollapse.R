
#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using all states and time periods with collapsed agegroups                                       
#
# Input:    regression_state_agecollapse.rda
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
load("./Data_ready/regression_state_agecollapse.rda")


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
#                                       Plotting temporal variation in mortality
#====================================================================================================


#====================================================================================================
#                                       Rescaling mortality to 100% - lags
#====================================================================================================


class(data)

data <- pdata.frame(data, index = c("ID", "year"))

# Graph with base 2007
data <- data %>%
  mutate(CVD_mort_100k_base2007 = 0)

data <- data %>% 
  mutate(CVD_mort_100k_base2007 = if_else(year == 2007, 1, CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2008, CVD_mort_100k/(lag(CVD_mort_100k)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2009, CVD_mort_100k/(lag(CVD_mort_100k, n = 2)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2010, CVD_mort_100k/(lag(CVD_mort_100k, n = 3)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2011, CVD_mort_100k/(lag(CVD_mort_100k, n = 4)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2012, CVD_mort_100k/(lag(CVD_mort_100k, n = 5)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2013, CVD_mort_100k/(lag(CVD_mort_100k, n = 6)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2014, CVD_mort_100k/(lag(CVD_mort_100k, n = 7)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2015, CVD_mort_100k/(lag(CVD_mort_100k, n = 8)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2016, CVD_mort_100k/(lag(CVD_mort_100k, n = 9)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2017, CVD_mort_100k/(lag(CVD_mort_100k, n = 10)), CVD_mort_100k_base2007),
         )

# Male
line_plot_base2007_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_base2007, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ region, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "") +
  labs(y = "%-Difference in CVD mortality compared to 2007") +
  geom_hline(yintercept = 1)

line_plot_base2007_male

# Female
line_plot_base2007_female <- data %>%
  filter(gender == "Female") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_base2007, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ region, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference in CVD mortality compared to 2007") +
  geom_hline(yintercept = 1)

line_plot_base2007_female



# Graph with percentage change compared to previous year
data <- data %>%
  mutate(CVD_mort_100k_percent = 0)

data <- data %>% 
  mutate(CVD_mort_100k_percent = if_else(year == 2007, 0, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2008, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2009, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2010, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2011, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2012, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2013, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2014, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2015, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2016, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2017, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
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
#                                       Plotting temporal variation in spending
#====================================================================================================


#====================================================================================================
#                                       Rescaling spending to 100% - lags
#====================================================================================================


# Graph with base 2007
data <- data %>%
  mutate(CVD_costs_100k_base2007 = 0)

data <- data %>% 
  mutate(CVD_costs_100k_base2007 = if_else(year == 2007, 1, CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2008, CVD_costs_100k/(lag(CVD_costs_100k)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2009, CVD_costs_100k/(lag(CVD_costs_100k, n = 2)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2010, CVD_costs_100k/(lag(CVD_costs_100k, n = 3)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2011, CVD_costs_100k/(lag(CVD_costs_100k, n = 4)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2012, CVD_costs_100k/(lag(CVD_costs_100k, n = 5)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2013, CVD_costs_100k/(lag(CVD_costs_100k, n = 6)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2014, CVD_costs_100k/(lag(CVD_costs_100k, n = 7)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2015, CVD_costs_100k/(lag(CVD_costs_100k, n = 8)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2016, CVD_costs_100k/(lag(CVD_costs_100k, n = 9)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2017, CVD_costs_100k/(lag(CVD_costs_100k, n = 10)), CVD_costs_100k_base2007),
  )

# Male
line_plot_base2007_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_costs_100k_base2007, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ region, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "") +
  labs(y = "%-Difference in CVD costsality compared to 2007") +
  geom_hline(yintercept = 1)

line_plot_base2007_male

# Female
line_plot_base2007_female <- data %>%
  filter(gender == "Female") %>% 
  ggplot(aes(x = year, y = CVD_costs_100k_base2007, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ region, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference in CVD costsality compared to 2007") +
  geom_hline(yintercept = 1)

line_plot_base2007_female



# Graph with percentage change compared to previous year
data <- data %>%
  mutate(CVD_costs_100k_percent = 0)

data <- data %>% 
  mutate(CVD_costs_100k_percent = if_else(year == 2007, 0, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2008, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2009, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2010, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2011, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2012, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2013, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2014, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2015, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2016, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2017, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
  )

# Male
line_plot_percent_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_costs_100k_percent, group = agegroup)) +
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
  ggplot(aes(x = year, y = CVD_costs_100k_percent, group = agegroup)) +
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

CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + region + agegroup*gender, data = data)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(CVD_reg)
vif(CVD_reg)

log_CVD_reg <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k+ year + region + agegroup*gender, data = data)
summary(log_CVD_reg)

coeftest(log_CVD_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(log_CVD_reg)
vif(log_CVD_reg)

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
# Outline:  Regression analysis using nielsenregion and all time periods with collapsed agegroups                                       
#
# Input:    regression_nreg_agecollapse.rda
# Output:   Table XX
#
#====================================================================================================
#====================================================================================================

# remove files from the R space
rm(list=ls( ))

load("./Data_ready/regression_nreg_agecollapse.rda")


#====================================================================================================
#                                       Conditioning on 60+
#====================================================================================================

data <- data_nreg

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

# Graph with base 2007
data <- data %>%
  mutate(CVD_mort_100k_base2007 = 0)

data <- data %>% 
  mutate(CVD_mort_100k_base2007 = if_else(year == 2007, 1, CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2008, CVD_mort_100k/(lag(CVD_mort_100k)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2009, CVD_mort_100k/(lag(CVD_mort_100k, n = 2)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2010, CVD_mort_100k/(lag(CVD_mort_100k, n = 3)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2011, CVD_mort_100k/(lag(CVD_mort_100k, n = 4)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2012, CVD_mort_100k/(lag(CVD_mort_100k, n = 5)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2013, CVD_mort_100k/(lag(CVD_mort_100k, n = 6)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2014, CVD_mort_100k/(lag(CVD_mort_100k, n = 7)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2015, CVD_mort_100k/(lag(CVD_mort_100k, n = 8)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2016, CVD_mort_100k/(lag(CVD_mort_100k, n = 9)), CVD_mort_100k_base2007),
         CVD_mort_100k_base2007 = if_else(year == 2017, CVD_mort_100k/(lag(CVD_mort_100k, n = 10)), CVD_mort_100k_base2007),
  )

# Male
line_plot_base2007_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_base2007, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ nielsenreg, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "") +
  labs(y = "%-Difference in CVD mortality compared to 2007") +
  geom_hline(yintercept = 1)

line_plot_base2007_male

# Female
line_plot_base2007_female <- data %>%
  filter(gender == "Female") %>% 
  ggplot(aes(x = year, y = CVD_mort_100k_base2007, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ nielsenreg, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference in CVD mortality compared to 2007") +
  geom_hline(yintercept = 1)

line_plot_base2007_female



# Graph with percentage change compared to previous year
data <- data %>%
  mutate(CVD_mort_100k_percent = 0)

data <- data %>% 
  mutate(CVD_mort_100k_percent = if_else(year == 2007, 0, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2008, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2009, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2010, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2011, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2012, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2013, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2014, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2015, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2016, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
         CVD_mort_100k_percent = if_else(year == 2017, (CVD_mort_100k/(lag(CVD_mort_100k)))-1, CVD_mort_100k_percent),
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
#                                       Plotting temporal variation in spending
#====================================================================================================


#====================================================================================================
#                                       Rescaling spending to 100% - lags
#====================================================================================================


# Graph with base 2007
data <- data %>%
  mutate(CVD_costs_100k_base2007 = 0)

data <- data %>% 
  mutate(CVD_costs_100k_base2007 = if_else(year == 2007, 1, CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2008, CVD_costs_100k/(lag(CVD_costs_100k)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2009, CVD_costs_100k/(lag(CVD_costs_100k, n = 2)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2010, CVD_costs_100k/(lag(CVD_costs_100k, n = 3)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2011, CVD_costs_100k/(lag(CVD_costs_100k, n = 4)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2012, CVD_costs_100k/(lag(CVD_costs_100k, n = 5)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2013, CVD_costs_100k/(lag(CVD_costs_100k, n = 6)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2014, CVD_costs_100k/(lag(CVD_costs_100k, n = 7)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2015, CVD_costs_100k/(lag(CVD_costs_100k, n = 8)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2016, CVD_costs_100k/(lag(CVD_costs_100k, n = 9)), CVD_costs_100k_base2007),
         CVD_costs_100k_base2007 = if_else(year == 2017, CVD_costs_100k/(lag(CVD_costs_100k, n = 10)), CVD_costs_100k_base2007),
  )

# Male
line_plot_base2007_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_costs_100k_base2007, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ nielsenreg, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "") +
  labs(y = "%-Difference in CVD costsality compared to 2007") +
  geom_hline(yintercept = 1)

line_plot_base2007_male

# Female
line_plot_base2007_female <- data %>%
  filter(gender == "Female") %>% 
  ggplot(aes(x = year, y = CVD_costs_100k_base2007, group = agegroup)) +
  geom_line(aes(colour = agegroup)) +
  facet_wrap(~ nielsenreg, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "") +
  labs(y = "%-Difference in CVD costsality compared to 2007") +
  geom_hline(yintercept = 1)

line_plot_base2007_female



# Graph with percentage change compared to previous year
data <- data %>%
  mutate(CVD_costs_100k_percent = 0)

data <- data %>% 
  mutate(CVD_costs_100k_percent = if_else(year == 2007, 0, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2008, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2009, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2010, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2011, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2012, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2013, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2014, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2015, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2016, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
         CVD_costs_100k_percent = if_else(year == 2017, (CVD_costs_100k/(lag(CVD_costs_100k)))-1, CVD_costs_100k_percent),
  )

# Male
line_plot_percent_male <- data %>%
  filter(gender == "Male") %>% 
  ggplot(aes(x = year, y = CVD_costs_100k_percent, group = agegroup)) +
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
  ggplot(aes(x = year, y = CVD_costs_100k_percent, group = agegroup)) +
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

CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k + year + nielsenreg + gender + agegroup, data = data)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC0")

par(mfrow=c(2,2))
plot(CVD_reg)
vif(CVD_reg)

log_CVD_reg <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k, data = data)
summary(log_CVD_reg)

coeftest(log_CVD_reg, vcov. = vcovHC, type = "HC0")

par(mfrow=c(2,2))
plot(log_CVD_reg)
vif(log_CVD_reg)

#====================================================================================================
#                                               Final cancer FD model
#====================================================================================================

cancer_reg <- lm(d_cancer_mort_100k ~ d_cancer_costs_100k + d_lag_cancer_costs_100k + year + gender, data = data)
summary(cancer_reg)

coeftest(cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(cancer_reg)
vif(cancer_reg)

log_cancer_reg <- lm(d_log_cancer_mort_100k ~ d_log_cancer_costs_100k + d_lag_log_cancer_costs_100k + year + gender, data = data)
summary(log_cancer_reg)

coeftest(log_cancer_reg, vcov. = vcovHC, type = "HC1")

par(mfrow=c(2,2))
plot(log_cancer_reg)
vif(log_cancer_reg)



#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Regression analysis using country level and all time periods with collapsed agegroups                                       
#
# Input:    regression_nreg_agecollapse.rda
# Output:   Table XX
#
#====================================================================================================
#====================================================================================================

# remove files from the R space
rm(list=ls( ))

load("./Data_ready/regression_state_agecollapse.rda")

#====================================================================================================
#                                       Reshape for collapsing data 
#====================================================================================================

data <- data_agecollapse

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


#====================================================================================================
#                                       Conditioning on 60+
#====================================================================================================


# Drop below 60, low variation in agegroups before (similar to van Baal et al., 2018), lot of noise
data <- data_wide %>%
  filter(agegroup =="6" | agegroup == "7" | agegroup =="8" | agegroup =="9") %>% 
  filter(!(agegroup =="9" & gender == "Male"))

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
#                                               Final CVD FD model
#====================================================================================================
library(car)

CVD_reg <- lm(d_CVD_mort_100k ~ d_CVD_costs_100k + d_lag_CVD_costs_100k, data = data)
summary(CVD_reg)

coeftest(CVD_reg, vcov. = vcovHC, type = "HC0")

par(mfrow=c(2,2))
plot(CVD_reg)
vif(CVD_reg)

log_CVD_reg <- lm(d_log_CVD_mort_100k ~ d_log_CVD_costs_100k + d_lag_log_CVD_costs_100k, data = data)
summary(log_CVD_reg)

coeftest(log_CVD_reg, vcov. = vcovHC, type = "HC0")

par(mfrow=c(2,2))
plot(log_CVD_reg)

