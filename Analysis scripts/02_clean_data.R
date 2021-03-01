#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Recode and clean data for analysis                                             
#
# Input:    full_analysis_data.rda
# Output:   clean_data.rda
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
library(collapse)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(spdep)
library(rgeos)
library(maptools)
library(sf)

load("./Data_ready/full_analysis_data.rda")


#====================================================================================================
#                                       Generate factor variables
#====================================================================================================


# Recode categorical data to factors, as better for regressions 
# (lm and plm can then automatically use dummy coding) and graphs

# Region
data <- data %>%
  mutate(region = as.factor(region)) %>%
  mutate(region = recode(region,
                             "8" = "BW",
                             "9" = "BY",
                             "11" = "BE",
                             "12" = "BB",
                             "4" = "HB",
                             "2" = "HH",
                             "6" = "HE",
                             "13" = "MV",
                             "3" = "NI",
                             "5" = "NW",
                             "7" = "RP",
                             "10" = "SL",
                             "14" = "SN",
                             "15" = "ST",
                             "1" = "SH",
                             "16" = "TH")
                             )
# Gender
data <- data %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(gender = recode(gender, "1" = "Male", "2" = "Female"))

# Period (year)
data <- data %>%
  mutate(year = as.factor(period)) %>% 
  mutate(year = recode(year,
                      "1" = "2007",
                      "2" ="2008",
                      "3" ="2009",
                      "4" ="2010",
                      "5" ="2011",
                      "6" ="2012",
                      "7" ="2013",
                      "8" ="2014",
                      "9" ="2015",
                      "10" ="2016",
                      "11" ="2017")) %>% select(-period)

# Age group
data <- data %>% 
  mutate(agegroup = as.factor(agegroup)) %>% 
  mutate(agegroup = recode(agegroup, '1' = "<1",
                         '2' = "1-14",
                         '3' = "15-19",
                         '4' = "20-24",
                         '5' = "25-29",
                         '6' = "30-34",
                         '7' = "35-39",
                         '8' = "40-44",
                         '9' = "45-49",
                         '10' = "50-54",
                         '11' = "55-59",
                         '12' = "60-64",
                         '13' = "65-69",
                         '14' = "70-74",
                         '15' = "75-79",
                         '16' = "80-84",
                         '17' = "85-89",
                         '18' = "90+"))


#====================================================================================================
#               Missing data for costs only occurring in agegroups below 60
#====================================================================================================


# Instances with less then 3 observations per cell were anonymised by the data holder using X
# Only occuring for cancer and CVD
ind1 <- is.na(data$cancer_costs) # logical TRUE/FALSE vector
ind2 <- is.na(data$cancer_N) # logical TRUE/FALSE vector

sum(ind1) 
sum(ind2)

ind1 <- is.na(data$CVD_costs) # logical TRUE/FALSE vector
ind2 <- is.na(data$CVD_N) # logical TRUE/FALSE vector

sum(ind1) 
sum(ind2)

ind1 <- is.na(data$resp_costs) # logical TRUE/FALSE vector
ind2 <- is.na(data$resp_N) # logical TRUE/FALSE vector

sum(ind1) 
sum(ind2)

ind1 <- is.na(data$tot_costs) # logical TRUE/FALSE vector
ind2 <- is.na(data$tot_N) # logical TRUE/FALSE vector

sum(ind1) 
sum(ind2)


#====================================================================================================
#                             Generate cost values per 100,000 population
#====================================================================================================

  
# Population variable based on calculation from mortality statitics which had cases per 100,000 and 
# the total number of cases

# Cost per 100,000
data <- data %>%
  mutate(overall_costs_100k = ((tot_costs*tot_N)/population)*100000,
         CVD_costs_100k = ((CVD_costs*CVD_N)/population)*100000,
         cancer_costs_100k = ((cancer_costs*cancer_N)/population)*100000,
         resp_costs_100k = ((resp_costs*resp_N)/population)*100000)

# Hospital cases per 100,000
data <- data %>%
  mutate(overall_hospcases_100k = (tot_N/population)*100000,
         CVD_hospcases_100k = (CVD_N/population)*100000,
         cancer_hospcases_100k = (cancer_N/population)*100000,
         resp_hospcases_100k = (resp_N/population)*100000)
      
# Rename average costs per hospital case
data <- data %>%
  rename(overall_avg_cost_case = tot_costs,
         CVD_avg_cost_case = CVD_costs,
         cancer_avg_cost_case = cancer_costs,
         resp_avg_cost_case = resp_costs)

# Rename tot_ columns
data <- data %>%
  rename(overall_deaths = tot_deaths,
         overall_mort_100k = tot_mort_100k,
         overall_N = tot_N)

#====================================================================================================
#                               Generate log costs and mortality
#====================================================================================================


# Observations with zero mortality will be kept but not used in base case log specification
# In alternative specification could add very small number to still use them
# Only occur in agegroups below 60

# Generate logs
data <- data %>%
  mutate(log_overall_costs_100k = log(overall_costs_100k),
         log_CVD_costs_100k = log(CVD_costs_100k),
         log_cancer_costs_100k = log(cancer_costs_100k),
         log_resp_costs_100k = log(resp_costs_100k),
         log_overall_mort_100k = log(overall_mort_100k),
         log_CVD_mort_100k = log(CVD_mort_100k),
         log_cancer_mort_100k = log(cancer_mort_100k),
         log_resp_mort_100k = log(resp_mort_100k))

# Replace -Inf by na
data <- data %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., -Inf)))


# Generating total costs variable
data <- data %>%
  mutate(overall_costs_sum = overall_avg_cost_case*overall_N,
         CVD_costs_sum = CVD_avg_cost_case*CVD_N,
         cancer_costs_sum = cancer_avg_cost_case*cancer_N,
         resp_costs_sum = resp_avg_cost_case*resp_N)

#====================================================================================================
#                                      Dropping males 90+
#====================================================================================================


# Drop 90+ men before 2011 due to comparability issues (after 2011 based on widely different population size estimate)
# https://www.destatis.de/DE/Methoden/WISTA-Wirtschaft-und-Statistik/2015/04/ermittlung-einwohnerzahlen-042015.pdf?__blob=publicationFile

data <- data %>%
  filter(!(agegroup =="90+" & gender =="Male")) 


#====================================================================================================
#                               Save data
#====================================================================================================


# Reorder columns
colnames(data)
data <- data[,c(21, 1:20,22:41)] # order columns

data <- data %>% mutate(id = rownames(data))

save(data, file = "./Data_ready/clean_data.rda")


#====================================================================================================
#                               Load shapefile for graphical analysis
#====================================================================================================


# Description of loading in shapefiles
# https://rstudio-pubs-static.s3.amazonaws.com/280176_81148aa4c2024d6ca6e9d21598a3e41f.html

# Recode state names
shp <- readOGR(dsn = "./Data_ready/Shapefile",layer = "DEU_adm1") # , stringsAsFactors = F) am ende

shp@data <- shp@data %>% mutate(NAME_1 = recode(NAME_1,
                                                "Baden-WÃ¼rttemberg" = "BW",
                                                "Bayern" = "BY",
                                                "Berlin" = "BE",
                                                "Brandenburg" = "BB",
                                                "Bremen" = "HB",
                                                "Hamburg" = "HH",
                                                "Hessen" = "HE",
                                                "Mecklenburg-Vorpommern" = "MV",
                                                "Niedersachsen" = "NI",
                                                "Nordrhein-Westfalen" = "NW",
                                                "Rheinland-Pfalz" = "RP",
                                                "Saarland" = "SL",
                                                "Sachsen" = "SN",
                                                "Sachsen-Anhalt" = "ST",
                                                "Schleswig-Holstein" = "SH",
                                                "ThÃ¼ringen" = "TH"
))

shp_df <- broom::tidy(shp, "NAME_1")

save(shp_df, file = "./Data_ready/shapefile.rda")



