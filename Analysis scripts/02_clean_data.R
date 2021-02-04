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


# banner("This text", emph = TRUE, bandChar = "=", leftSideHashes = 1, rightSideHashes = 0, minHashes = 100)


#====================================================================================================
#                                       Load R packages and data                                          
#====================================================================================================


# banner("This text", bandChar = "=", leftSideHashes = 1, rightSideHashes = 0, minHashes = 100, center = TRUE)

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
#                                       Missing data
#====================================================================================================


# Instances with less then 3 observations per cell were anonymised by the FDZ using X
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
#                             Generate values per 100,000 population
#====================================================================================================

  
# Population variable based on calculation from mortality statitics which had cases per 100,000 and 
# the total number of cases

data <- data %>%
  mutate(tot_costs_100k = (tot_costs/population)*100000,
         CVD_costs_100k = (CVD_costs/population)*100000,
         cancer_costs_100k = (cancer_costs/population)*100000,
         resp_costs_100k = (resp_costs/population)*100000)


data <- data %>%
  mutate(tot_N_100k = (tot_N/population)*100000,
         CVD_N_100k = (CVD_N/population)*100000,
         cancer_N_100k = (cancer_N/population)*100000,
         resp_N_100k = (resp_N/population)*100000)
      

#====================================================================================================
#                               Generate log costs and mortality
#====================================================================================================


data <- data %>%
  mutate(log_tot_costs_100k = log(tot_costs_100k),
         log_CVD_costs_100k = log(CVD_costs_100k),
         log_cancer_costs_100k = log(cancer_costs_100k),
         log_resp_costs_100k = log(resp_costs_100k),
         log_tot_mort_100k = log(tot_mort_100k),
         log_CVD_mort_100k = log(CVD_mort_100k),
         log_cancer_mort_100k = log(cancer_mort_100k),
         log_resp_mort_100k = log(resp_mort_100k))

# Total costs
data <- data %>%
  mutate(total_costs_sum = tot_costs*tot_N,
         CVD_costs_sum = CVD_costs*CVD_N,
         cancer_costs_sum = cancer_costs*cancer_N,
         resp_costs_sum = resp_costs*resp_N)

#====================================================================================================
#                               Save data
#====================================================================================================


# Reorder columns

colnames(data)
data <- data[,c(21, 1:20,22:41)] # order columns

data <- data %>% mutate(id = rownames(data))

save(data, file = "./Data_ready/clean_data.rda")


#====================================================================================================
#                               Combine regions into super-regions (Nielsen regions)
# Rationale is size of regions very different, but treated the same in regression (without weights)
# At the same time, very low number of observations for smaller regions
#====================================================================================================



# Reshape data to wide format

data_wide <- data %>% 
  subset(select = c(year, region, gender,agegroup, CVD_costs))

data_wide <- data_wide %>%
  pivot_wider(names_from = year, values_from = CVD_costs, names_prefix = "y")

data_wide <- data_wide %>%
  subset(select = c(region, gender,agegroup, y2007, y2017)) %>%
  filter(!is.na(y2007) | !is.na(y2017))












#====================================================================================================
#                               Load shapefile
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



