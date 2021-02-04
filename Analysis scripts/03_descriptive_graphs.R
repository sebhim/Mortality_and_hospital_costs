#====================================================================================================
#====================================================================================================
#                                                                                                   
# Outline:  Descriptive and graphical analysis                                            
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
library(RColorBrewer)
library(grid)
library(gridExtra)
library(patchwork)

# Packages for maps and spatial data
library(rgdal)
library(spdep)
library(rgeos)
library(maptools)
library(sf)
library(ggplot2)
library(viridis)

load("./Data_ready/clean_data.rda")


#====================================================================================================
#                                       Mortality figure
#====================================================================================================

# Deaths over 90 trend

deaths_over_90 <- data %>% 
  filter(agegroup =="90+") %>% 
  group_by(gender, year) %>%
  summarise(total_deaths = sum(tot_deaths))

deaths_over_90_graph <- deaths_over_90 %>% 
  ggplot(aes(year, total_deaths)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Total deaths 90+") +theme(legend.position = "none")
deaths_over_90_graph

# Population over 90 trend
pop_over_90 <- data %>% 
  filter(agegroup =="90+") %>% 
  group_by(gender, year) %>%
  summarise(population = sum(population)/1000)

pop_over_90_graph <- pop_over_90 %>% 
  ggplot(aes(year, population)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Total N 90+") +theme(legend.position = "none")
pop_over_90_graph



# Total  
mort_100k_2007_tot <- data %>%
  filter(year =="2007" | year == "2017") %>% 
  group_by(agegroup, gender, year) %>%
  summarise(tot_deaths = sum(tot_deaths), pop = sum(population)) %>% 
  mutate(tot_mortality = tot_deaths/pop)


m1 <- mort_100k_2007_tot %>% 
  filter((agegroup != "<1" &
            agegroup != "1-14" &
            agegroup != "15-19" &
            agegroup != "20-24" &
            agegroup != "25-29" &
            agegroup != "30-34" &
            agegroup != "35-39" &
            agegroup != "40-44" &
            agegroup != "45-49" &
            agegroup != "50-54" &
            agegroup != "55-59")) %>% 
  ggplot(aes(agegroup, tot_mortality, color = year, shape = year)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Total mortality rate") +
  theme(legend.position = c(0.1,0.8), 
        legend.background = element_rect(fill="white", colour = "black"), 
        legend.title=element_blank(), axis.title = element_blank())

# CVD
mort_100k_2007_CVD <- data %>%
  filter(year =="2007" | year == "2017") %>% 
  group_by(agegroup, gender, year) %>%
  summarise(CVD_deaths = sum(CVD_deaths), pop = sum(population)) %>% 
  mutate(CVD_mortality = CVD_deaths/pop)


m2 <- mort_100k_2007_CVD %>% 
  filter((agegroup != "<1" &
            agegroup != "1-14" &
            agegroup != "15-19" &
            agegroup != "20-24" &
            agegroup != "25-29" &
            agegroup != "30-34" &
            agegroup != "35-39" &
            agegroup != "40-44" &
            agegroup != "45-49" &
            agegroup != "50-54" &
            agegroup != "55-59")) %>% 
  ggplot(aes(agegroup, CVD_mortality, color = year, shape = year)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("CVD mortality rate") +theme(legend.position = "none")

# Cancer
mort_100k_2007_cancer <- data %>%
  filter(year =="2007" | year == "2017") %>% 
  group_by(agegroup, gender, year) %>%
  summarise(cancer_deaths = sum(cancer_deaths), pop = sum(population)) %>% 
  mutate(cancer_mortality = cancer_deaths/pop)


m3 <- mort_100k_2007_cancer %>% 
  filter((agegroup != "<1" &
            agegroup != "1-14" &
            agegroup != "15-19" &
            agegroup != "20-24" &
            agegroup != "25-29" &
            agegroup != "30-34" &
            agegroup != "35-39" &
            agegroup != "40-44" &
            agegroup != "45-49" &
            agegroup != "50-54" &
            agegroup != "55-59")) %>% 
  ggplot(aes(agegroup, cancer_mortality, color = year, shape = year)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Cancer mortality rate") +theme(legend.position = "none")

# Resp
mort_100k_2007_resp <- data %>%
  filter(year =="2007" | year == "2017") %>% 
  group_by(agegroup, gender, year) %>%
  summarise(resp_deaths = sum(resp_deaths), pop = sum(population)) %>% 
  mutate(resp_mortality = resp_deaths/pop)

m4 <- mort_100k_2007_resp %>% 
  filter((agegroup != "<1" &
            agegroup != "1-14" &
            agegroup != "15-19" &
            agegroup != "20-24" &
            agegroup != "25-29" &
            agegroup != "30-34" &
            agegroup != "35-39" &
            agegroup != "40-44" &
            agegroup != "45-49" &
            agegroup != "50-54" &
            agegroup != "55-59")) %>% 
  ggplot(aes(agegroup, resp_mortality, color = year, shape = year)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank())  +
  ggtitle("Resp. mortality rate") +theme(legend.position = "none")



# Check outlier
mort_100k_2007_resp_test <- data %>%
  filter(year =="2007" | year == "2008" | year == "2009" | year == "20") %>% 
  group_by(region, agegroup, gender, year) %>%
  summarise(resp_deaths = sum(resp_deaths), pop = sum(population)) %>% 
  mutate(resp_mortality = resp_deaths/pop)



m_fig <- grid.arrange(m1,m2,m3,m4, ncol = 2)

#====================================================================================================
#                                       Cost figure
#====================================================================================================


# Total
cost_100k_2007_tot <- data %>%
  filter(year =="2007" | year == "2017") %>% 
  group_by(agegroup, gender, year) %>%
  summarise(total_costs_pc = sum(total_costs_sum)/sum(population))


c1 <- cost_100k_2007_tot %>% 
  filter((agegroup != "<1" &
            agegroup != "1-14" &
            agegroup != "15-19" &
            agegroup != "20-24" &
            agegroup != "25-29" &
            agegroup != "30-34" &
            agegroup != "35-39" &
            agegroup != "40-44" &
            agegroup != "45-49" &
            agegroup != "50-54" &
            agegroup != "55-59")) %>% 
  ggplot(aes(agegroup, total_costs_pc, color = year, shape = year)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Total spending per capita in Euro")+ theme(legend.position = c(0.1,0.8), 
                                                 legend.background = element_rect(fill="white", colour = "black"), 
                                                 legend.title=element_blank()) 

# CVD
cost_100k_2007_CVD <- data %>%
  filter(year =="2007" | year == "2017") %>% 
  group_by(agegroup, gender, year) %>%
  summarise(CVD_costs_pc = sum(CVD_costs_sum)/sum(population))

            
c2 <- cost_100k_2007_CVD %>% 
  filter((agegroup != "<1" &
            agegroup != "1-14" &
            agegroup != "15-19" &
            agegroup != "20-24" &
            agegroup != "25-29" &
            agegroup != "30-34" &
            agegroup != "35-39" &
            agegroup != "40-44" &
            agegroup != "45-49" &
            agegroup != "50-54" &
            agegroup != "55-59")) %>% 
  ggplot(aes(agegroup, CVD_costs_pc, color = year, shape = year)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("CVD spending per capita in Euro") +theme(legend.position = "none")

# cancer
cost_100k_2007_cancer <- data %>%
  filter(year =="2007" | year == "2017") %>% 
  group_by(agegroup, gender, year) %>%
  summarise(cancer_costs_pc = sum(cancer_costs_sum, na.rm = TRUE)/sum(population))


# 2007 cancer cost NA for >90
ind <- is.na(cost_100k_2007_cancer$cancer_costs_pc)
sum(ind)

test <- data %>%
  filter((year =="2007" | year == "2008" |  year =="2009") & region == "SL") %>% select(cancer_costs_sum, population, year, agegroup, region, gender)





c3 <- cost_100k_2007_cancer %>% 
  filter((agegroup != "<1" &
            agegroup != "1-14" &
            agegroup != "15-19" &
            agegroup != "20-24" &
            agegroup != "25-29" &
            agegroup != "30-34" &
            agegroup != "35-39" &
            agegroup != "40-44" &
            agegroup != "45-49" &
            agegroup != "50-54" &
            agegroup != "55-59")) %>% 
  ggplot(aes(agegroup, cancer_costs_pc, color = year, shape = year)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Cancer spending per capita in Euro") +theme(legend.position = "none")

# Resp
cost_100k_2007_resp <- data %>%
  filter(year =="2007" | year == "2017") %>% 
  group_by(agegroup, gender, year) %>%
  summarise(resp_costs_pc = sum(resp_costs_sum)/sum(population))


c4 <- cost_100k_2007_resp %>% 
  filter((agegroup != "<1" &
            agegroup != "1-14" &
            agegroup != "15-19" &
            agegroup != "20-24" &
            agegroup != "25-29" &
            agegroup != "30-34" &
            agegroup != "35-39" &
            agegroup != "40-44" &
            agegroup != "45-49" &
            agegroup != "50-54" &
            agegroup != "55-59")) %>% 
  ggplot(aes(agegroup, resp_costs_pc, color = year, shape = year)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Resp. spending per capita in Euro") +theme(legend.position = "none")

c_fig <- grid.arrange(c1,c2,c3, c4, ncol = 2)


#====================================================================================================
#                                       Mortality time trend 70-74
#====================================================================================================


# Total
mt1_total <- data %>% 
  filter(agegroup =="70-74") %>% 
  group_by(gender, year) %>%
  summarise(total_deaths = sum(tot_deaths), pop = sum(population)) %>% 
  mutate(total_mortality = total_deaths/pop)

mt1 <- mt1_total %>% 
  ggplot(aes(year, total_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Total mortality rate 70-74") +theme(legend.position = "none")


# CVD
mt2_CVD <- data %>% 
  filter(agegroup =="70-74") %>% 
  group_by(gender, year) %>%
  summarise(CVD_deaths = sum(CVD_deaths), pop = sum(population)) %>% 
  mutate(CVD_mortality = CVD_deaths/pop)

mt2 <- mt2_CVD %>% 
  ggplot(aes(year, CVD_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("CVD mortality rate 70-74") +theme(legend.position = "none")


# Cancer
mt3_cancer <- data %>% 
  filter(agegroup =="70-74") %>% 
  group_by(gender, year) %>%
  summarise(cancer_deaths = sum(cancer_deaths), pop = sum(population)) %>% 
  mutate(cancer_mortality = cancer_deaths/pop)

mt3 <- mt3_cancer %>%
  ggplot(aes(year, cancer_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Cancer mortality rate 70-74") +theme(legend.position = "none")


# Resp
mt4_resp <- data %>% 
    filter(agegroup =="70-74") %>% 
    group_by(gender, year) %>%
    summarise(resp_deaths = sum(resp_deaths), pop = sum(population)) %>% 
    mutate(resp_mortality = resp_deaths/pop)

mt4 <- mt4_resp %>% 
  ggplot(aes(year, resp_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Resp. mortality rate 70-74") +theme(legend.position = "none")

mt_fig_70_74 <- grid.arrange(mt1,mt2,mt3, mt4, ncol = 2)


#====================================================================================================
#                                       Mortality time trend 75-79
#====================================================================================================


# Total
mt1_total <- data %>% 
  filter(agegroup =="75-79") %>% 
  group_by(gender, year) %>%
  summarise(total_deaths = sum(tot_deaths), pop = sum(population)) %>% 
  mutate(total_mortality = total_deaths/pop)

mt1 <- mt1_total %>% 
  ggplot(aes(year, total_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Total mortality rate 75-79") +theme(legend.position = "none")


# CVD
mt2_CVD <- data %>% 
  filter(agegroup =="75-79") %>% 
  group_by(gender, year) %>%
  summarise(CVD_deaths = sum(CVD_deaths), pop = sum(population)) %>% 
  mutate(CVD_mortality = CVD_deaths/pop)

mt2 <- mt2_CVD %>% 
  ggplot(aes(year, CVD_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("CVD mortality rate 75-79") +theme(legend.position = "none")


# Cancer
mt3_cancer <- data %>% 
  filter(agegroup =="75-79") %>% 
  group_by(gender, year) %>%
  summarise(cancer_deaths = sum(cancer_deaths), pop = sum(population)) %>% 
  mutate(cancer_mortality = cancer_deaths/pop)

mt3 <- mt3_cancer %>%
  ggplot(aes(year, cancer_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Cancer mortality rate 75-79") +theme(legend.position = "none")


# Resp
mt4_resp <- data %>% 
  filter(agegroup =="75-79") %>% 
  group_by(gender, year) %>%
  summarise(resp_deaths = sum(resp_deaths), pop = sum(population)) %>% 
  mutate(resp_mortality = resp_deaths/pop)

mt4 <- mt4_resp %>% 
  ggplot(aes(year, resp_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Resp. mortality rate 75-79") +theme(legend.position = "none")

mt_fig_75_79 <- grid.arrange(mt1,mt2,mt3, mt4, ncol = 2)


#====================================================================================================
#                                       Mortality time trend 80-84
#====================================================================================================


# Total
mt1_total <- data %>% 
  filter(agegroup =="80-84") %>% 
  group_by(gender, year) %>%
  summarise(total_deaths = sum(tot_deaths), pop = sum(population)) %>% 
  mutate(total_mortality = total_deaths/pop)

mt1 <- mt1_total %>% 
  ggplot(aes(year, total_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Total mortality rate 80-84") +theme(legend.position = "none")


# CVD
mt2_CVD <- data %>% 
  filter(agegroup =="80-84") %>% 
  group_by(gender, year) %>%
  summarise(CVD_deaths = sum(CVD_deaths), pop = sum(population)) %>% 
  mutate(CVD_mortality = CVD_deaths/pop)

mt2 <- mt2_CVD %>% 
  ggplot(aes(year, CVD_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("CVD mortality rate 80-84") +theme(legend.position = "none")


# Cancer
mt3_cancer <- data %>% 
  filter(agegroup =="80-84") %>% 
  group_by(gender, year) %>%
  summarise(cancer_deaths = sum(cancer_deaths), pop = sum(population)) %>% 
  mutate(cancer_mortality = cancer_deaths/pop)

mt3 <- mt3_cancer %>%
  ggplot(aes(year, cancer_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Cancer mortality rate 80-84") +theme(legend.position = "none")


# Resp
mt4_resp <- data %>% 
  filter(agegroup =="80-84") %>% 
  group_by(gender, year) %>%
  summarise(resp_deaths = sum(resp_deaths), pop = sum(population)) %>% 
  mutate(resp_mortality = resp_deaths/pop)

mt4 <- mt4_resp %>% 
  ggplot(aes(year, resp_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Resp. mortality rate 80-84") +theme(legend.position = "none")

mt_fig_80_84 <- grid.arrange(mt1,mt2,mt3, mt4, ncol = 2)

#====================================================================================================
#                                       Mortality time trend 85-89
#====================================================================================================


# Total
mt1_total <- data %>% 
  filter(agegroup =="85-89") %>% 
  group_by(gender, year) %>%
  summarise(total_deaths = sum(tot_deaths), pop = sum(population)) %>% 
  mutate(total_mortality = total_deaths/pop)

mt1 <- mt1_total %>% 
  ggplot(aes(year, total_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Total mortality rate 85-89") +theme(legend.position = "none")


# CVD
mt2_CVD <- data %>% 
  filter(agegroup =="85-89") %>% 
  group_by(gender, year) %>%
  summarise(CVD_deaths = sum(CVD_deaths), pop = sum(population)) %>% 
  mutate(CVD_mortality = CVD_deaths/pop)

mt2 <- mt2_CVD %>% 
  ggplot(aes(year, CVD_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("CVD mortality rate 85-89") +theme(legend.position = "none")


# Cancer
mt3_cancer <- data %>% 
  filter(agegroup =="85-89") %>% 
  group_by(gender, year) %>%
  summarise(cancer_deaths = sum(cancer_deaths), pop = sum(population)) %>% 
  mutate(cancer_mortality = cancer_deaths/pop)

mt3 <- mt3_cancer %>%
  ggplot(aes(year, cancer_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Cancer mortality rate 85-89") +theme(legend.position = "none")


# Resp
mt4_resp <- data %>% 
  filter(agegroup =="85-89") %>% 
  group_by(gender, year) %>%
  summarise(resp_deaths = sum(resp_deaths), pop = sum(population)) %>% 
  mutate(resp_mortality = resp_deaths/pop)

mt4 <- mt4_resp %>% 
  ggplot(aes(year, resp_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Resp. mortality rate 85-89") +theme(legend.position = "none")

mt_fig_85_89 <- grid.arrange(mt1,mt2,mt3, mt4, ncol = 2)

#====================================================================================================
#                                       Mortality time trend 90+
#====================================================================================================


# Total
mt1_total <- data %>% 
  filter(agegroup =="90+") %>% 
  group_by(gender, year) %>%
  summarise(total_deaths = sum(tot_deaths), pop = sum(population)) %>% 
  mutate(total_mortality = total_deaths/pop)

mt1 <- mt1_total %>% 
  ggplot(aes(year, total_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Total mortality rate 90+") +theme(legend.position = "none")


# CVD
mt2_CVD <- data %>% 
  filter(agegroup =="90+") %>% 
  group_by(gender, year) %>%
  summarise(CVD_deaths = sum(CVD_deaths), pop = sum(population)) %>% 
  mutate(CVD_mortality = CVD_deaths/pop)

mt2 <- mt2_CVD %>% 
  ggplot(aes(year, CVD_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("CVD mortality rate 90+") +theme(legend.position = "none")


# Cancer
mt3_cancer <- data %>% 
  filter(agegroup =="90+") %>% 
  group_by(gender, year) %>%
  summarise(cancer_deaths = sum(cancer_deaths), pop = sum(population)) %>% 
  mutate(cancer_mortality = cancer_deaths/pop)

mt3 <- mt3_cancer %>%
  ggplot(aes(year, cancer_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Cancer mortality rate 90+") +theme(legend.position = "none")


# Resp
mt4_resp <- data %>% 
  filter(agegroup =="90+") %>% 
  group_by(gender, year) %>%
  summarise(resp_deaths = sum(resp_deaths), pop = sum(population)) %>% 
  mutate(resp_mortality = resp_deaths/pop)

mt4 <- mt4_resp %>% 
  ggplot(aes(year, resp_mortality)) +
  geom_point(size = 2) + facet_grid(.~gender) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  ggtitle("Resp. mortality rate 90+") +theme(legend.position = "none")

mt_fig_90 <- grid.arrange(mt1,mt2,mt3, mt4, ncol = 2)


#====================================================================================================
#                                       Load in shapefile for Germany
#====================================================================================================


# Description of loading in shapefiles
# https://rstudio-pubs-static.s3.amazonaws.com/280176_81148aa4c2024d6ca6e9d21598a3e41f.html

# Recode state names

shp <- readOGR(dsn = "./Data_ready/Shapefile",layer = "DEU_adm1", stringsAsFactors = F)

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

# Plain map Germany
# To center labels:
# https://stackoverflow.com/questions/9441436/ggplot-centered-names-on-a-map/30305806

map <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group), colour = "grey", fill = NA)
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=function(x)mean(range(x)))
map + geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 2) + theme_void() + coord_quickmap()


#====================================================================================================
#                                       Mortality figure regions
#====================================================================================================


#====================================================================================================
#                                       Total mortality
#====================================================================================================


# Mortality total 2007  data
mort100k_state_tot <- data %>%
  filter((year =="2007" | year =="2017")) %>% 
  group_by(region, year) %>%
  summarise(tot_deaths = sum(tot_deaths), pop = sum(population)) %>% 
  mutate(tot_mortality = (tot_deaths/pop)*100000) %>% mutate(id = region) %>% select(id, year, tot_mortality) 




# Merge data

# https://gis.stackexchange.com/questions/259803/merge-spatial-and-non-spatial-data-and-create-spatialpolygonsdataframe-in-r
merge <- merge(shp_df, mort100k_state_tot) 

# Fill 

# https://stackoverflow.com/questions/47665971/how-to-fill-a-map-from-a-shapefile-using-its-own-attributes-table-in-r

tot_2007 <- merge %>% filter(year =="2007") 
tot_2007 <- tot_2007 %>% ggplot() + 
  geom_polygon(data = tot_2007, aes(x = long, y = lat, group = group, color = "grey", fill = tot_mortality), 
               colour = "grey", size = 0.5) +
               theme_void() + 
               coord_quickmap() +
               geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 2) +
  scale_fill_gradient(low = "grey90", high = "red4", limits = c(850, 1500)) +
  ggtitle("Total mortality per 100,000: 2007") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

tot_2007

tot_2017 <- merge %>% filter(year =="2017") 
tot_2017 <- tot_2017 %>% ggplot() + 
  geom_polygon(data = tot_2017, aes(x = long, y = lat, group = group, color = "grey", fill = tot_mortality), 
               colour = "grey", size = 0.5) +
  theme_void() + 
  coord_quickmap() +
  geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 2) +
  scale_fill_gradient(low = "grey90", high = "red4", limits = c(850, 1500)) +
  ggtitle("Total mortality per 100,000: 2017") +  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))
  


tot_2017
map_tot <- grid.arrange(tot_2007, tot_2017, ncol = 2)


#====================================================================================================
#                                       CVD mortality
#====================================================================================================


# Mortality CVD 2007  data
mort100k_state_CVD <- data %>%
  filter(year =="2007" | year =="2017") %>% 
  group_by(region, year) %>%
  summarise(CVD_deaths = sum(CVD_deaths), pop = sum(population)) %>% 
  mutate(CVD_mortality = (CVD_deaths/pop)*100000) %>% mutate(id = region) %>% select(id, year, CVD_mortality) 




# Merge data

# https://gis.stackexchange.com/questions/259803/merge-spatial-and-non-spatial-data-and-create-spatialpolygonsdataframe-in-r
merge <- merge(shp_df, mort100k_state_CVD) 

# Fill 

# https://stackoverflow.com/questions/47665971/how-to-fill-a-map-from-a-shapefile-using-its-own-attributes-table-in-r

CVD_2007 <- merge %>% filter(year =="2007") 
CVD_2007 <- CVD_2007 %>% ggplot() + 
  geom_polygon(data = CVD_2007, aes(x = long, y = lat, group = group, color = "grey", fill = CVD_mortality), 
               colour = "grey", size = 0.5) +
  theme_void() + 
  coord_quickmap() +
  geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 2) 

CVD_2007

CVD_2017 <- merge %>% filter(year =="2017") 
CVD_2017 <- CVD_2017 %>% ggplot() + 
  geom_polygon(data = CVD_2007, aes(x = long, y = lat, group = group, color = "grey", fill = CVD_mortality), 
               colour = "grey", size = 0.5) +
  theme_void() + 
  coord_quickmap() +
  geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 2) 


CVD_2017
map_CVD <- grid.arrange(CVD_2007, CVD_2017, ncol = 2)



#====================================================================================================
#                                       Cost figure regions
#====================================================================================================


# Costs total 2007  data
cost_state_2007_tot <- data %>%
  filter(year =="2007" | year == "2017") %>% 
  group_by(region, year) %>%
  summarise(total_costs_pc = sum(total_costs_sum)/sum(population))  %>%
  mutate(id = region) %>% 
  select(id, year, total_costs_pc) 


# Merge data
merge <- merge(shp_df, cost_state_2007_tot) 

# Figure
tot_2007 <- merge %>% filter(year =="2007") 
tot_2007 <- tot_2007 %>% ggplot() + 
  geom_polygon(data = merge, aes(x = long, y = lat, group = group, color = "grey", fill = total_costs_pc), 
               colour = "grey", size = 0.5) +
  theme_void() + 
  coord_quickmap() +
  geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 2) +
  scale_fill_gradient(low = "grey90", high = "dodgerblue4", limits = c(450, 1050)) +
  ggtitle("Total hospital spending per capita: 2007") +  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))



tot_2007

tot_2017 <- merge %>% filter(year =="2017") 
tot_2017 <- tot_2017 %>% ggplot() + 
  geom_polygon(data = tot_2017, aes(x = long, y = lat, group = group, color = "grey", fill = total_costs_pc), 
      colour = "grey", size = 0.5) +
      theme_void() + 
      coord_quickmap() +
      geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 2) +
      scale_fill_gradient(low = "grey90", high = "dodgerblue4", limits = c(450, 1050)) +
  ggtitle("Total hospital spending per capita: 2017") +  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))
  

tot_2017
map_tot <- grid.arrange(tot_2007, tot_2017, ncol = 2)


#====================================================================================================
#                                       Heatmap differences regions
#====================================================================================================


#====================================================================================================
#                                       CVD mortality change
#====================================================================================================


# Reshape data to wide format

data_wide <- data %>% 
  subset(select = c(year, region, gender,agegroup, CVD_mort_100k, CVD_costs_100k))

data_wide <- data_wide %>%
  pivot_wider(names_from = year, values_from = CVD_mort_100k, names_prefix = "y")

data_wide <- data_wide %>%
  subset(select = c(region, gender,agegroup, y2007, y2017)) %>%
  filter(!is.na(y2007) | !is.na(y2017))


data_wide_2007 <- data_wide %>%
  subset(select = -c(y2017)) %>% 
  filter(!is.na(y2007))
data_wide_2017 <- data_wide %>%
  subset(select = -c(y2007)) %>% 
  filter(!is.na(y2017))

# Merge
data_wide <- left_join(data_wide_2007, data_wide_2017, by = c("region", "gender", "agegroup"))

# Generate change in mortality per 100k from 2007 to 2017 per age, gender, region
data_wide <- data_wide %>%
  mutate(change_mort = y2017-y2007)

# Generate change in mortality in % (downside 0 observations are dropped)
data_wide <- data_wide %>%
  mutate(change_mort_percent = -1*((1-(y2017/y2007))*100))


#====================================================================================================
#                                       Male
#====================================================================================================


str(data_wide)

# If all age groups should be included

# data_wide %>%
#   ggplot(aes(x = region, y = agegroup)) +
#   geom_tile(aes(fill = change_mort_percent)) +
#   scale_fill_gradient2(limits=c(-100, 100), breaks=seq(-100,100,by=25)) 

# Just include older age groups where differences are larger
data_wide_m_old <- data_wide %>% 
  filter(gender == "Male") %>%
  filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+")

# Graph male
mort_reg_m <- data_wide_m_old %>%
  ggplot(aes(x = region, y = agegroup)) +
  geom_tile(aes(fill = change_mort)) +
  scale_fill_viridis_c(limits=c(-3100, 100), breaks=seq(-3000,0,by=1000)) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.7, "cm")) +
  labs(fill = "") +
  ggtitle("Male")+
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))

mort_reg_m

#====================================================================================================
#                                      Female
#====================================================================================================

# Just include older age groups where differences are larger, censor data at -3000 for figure
data_wide_f_old <- data_wide %>% 
  filter(gender == "Female") %>%
  filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+") %>% 
  mutate(change_mort = replace(change_mort, change_mort < -3000, -3000))

# Graph female
mort_reg_f <- data_wide_f_old %>%
  ggplot(aes(x = region, y = agegroup)) + 
  geom_tile(aes(fill = change_mort)) + 
  scale_fill_viridis_c(limits=c(-3100, 100), breaks=seq(-3000,0,by=1000)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.7, "cm"))+
  labs(fill = "") +
  ggtitle("Female") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5))


mort_reg_f

# Combine graph
mort_reg <- mort_reg_m + mort_reg_f & theme(legend.position = "bottom")
mort_reg <- mort_reg + plot_layout(guides = "collect") 

mort_reg


#====================================================================================================
#                                       CVD costs change
#====================================================================================================


# Reshape data to wide format

data_wide <- data %>% 
  subset(select = c(year, region, gender,agegroup, CVD_costs))

data_wide <- data_wide %>%
  pivot_wider(names_from = year, values_from = CVD_costs, names_prefix = "y")

data_wide <- data_wide %>%
  subset(select = c(region, gender,agegroup, y2007, y2017)) %>%
  filter(!is.na(y2007) | !is.na(y2017))


# data_wide_2007 <- data_wide %>%
#   subset(select = -c(y2017)) %>% 
#   filter(!is.na(y2007))
# data_wide_2017 <- data_wide %>%
#   subset(select = -c(y2007)) %>% 
#   filter(!is.na(y2017))
# 
# # Merge
# data_wide <- left_join(data_wide_2007, data_wide_2017, by = c("region", "gender", "agegroup"))

# Generate change in mortality per 100k from 2007 to 2017 per age, gender, region
data_wide <- data_wide %>%
  mutate(change_costs = y2017-y2007)

# Generate change in mortality in % (downside 0 observations are dropped)
data_wide <- data_wide %>%
  mutate(change_costs_percent = -1*((1-(y2017/y2007))*100))


#====================================================================================================
#                                       Male
#====================================================================================================


str(data_wide)

# If all age groups should be included

# data_wide %>%
#   ggplot(aes(x = region, y = agegroup)) +
#   geom_tile(aes(fill = change_mort_percent)) +
#   scale_fill_gradient2(limits=c(-100, 100), breaks=seq(-100,100,by=25)) 

# Just include older age groups where differences are larger
data_wide_m_old <- data_wide %>% 
  filter(gender == "Male") %>%
  filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+" )

# Graph male
costs_reg_m <- data_wide_m_old %>%
  ggplot(aes(x = region, y = agegroup)) +
  geom_tile(aes(fill = change_costs)) +
  scale_fill_viridis(option = "magma", direction = -1, limits=c(-100, 1900), breaks=seq(0,2000,by=500)) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.7, "cm")) +
  labs(fill = "") +
  ggtitle("Male")+
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))

costs_reg_m


#====================================================================================================
#                                      Female
#====================================================================================================

# Just include older age groups where differences are larger, censor data at -3000 for figure
data_wide_f_old <- data_wide %>% 
  filter(gender == "Female") %>%
  filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+") #%>% 
  #mutate(change_costs = replace(change_costs, change_costs < -3000, -3000))

# Graph female
costs_reg_f <- data_wide_f_old %>%
  ggplot(aes(x = region, y = agegroup)) + 
  geom_tile(aes(fill = change_costs)) + 
  scale_fill_viridis(option = "magma", direction = -1, limits=c(-100, 1900), breaks=seq(0,2000,by=500)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.7, "cm"))+
  labs(fill = "") +
  ggtitle("Female") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5))


costs_reg_f

# Combine graph
costs_reg <- costs_reg_m + costs_reg_f & theme(legend.position = "bottom")
costs_reg <- costs_reg + plot_layout(guides = "collect") 

costs_reg

mort_reg_m + mort_reg_f + costs_reg_m + costs_reg_f +
  plot_layout(guides = "collect") & theme(legend.position = "right")





#====================================================================================================
#                                       Heatmap differences regions
#====================================================================================================


#====================================================================================================
#                                       cancer mortality change
#====================================================================================================


# Reshape data to wide format

data_wide <- data %>% 
  subset(select = c(year, region, gender,agegroup, cancer_mort_100k, cancer_costs_100k))

data_wide <- data_wide %>%
  pivot_wider(names_from = year, values_from = cancer_mort_100k, names_prefix = "y")

data_wide <- data_wide %>%
  subset(select = c(region, gender,agegroup, y2007, y2017)) %>%
  filter(!is.na(y2007) | !is.na(y2017))


data_wide_2007 <- data_wide %>%
  subset(select = -c(y2017)) %>% 
  filter(!is.na(y2007))
data_wide_2017 <- data_wide %>%
  subset(select = -c(y2007)) %>% 
  filter(!is.na(y2017))

# Merge
data_wide <- left_join(data_wide_2007, data_wide_2017, by = c("region", "gender", "agegroup"))

# Generate change in mortality per 100k from 2007 to 2017 per age, gender, region
data_wide <- data_wide %>%
  mutate(change_mort = y2017-y2007)

# Generate change in mortality in % (downside 0 observations are dropped)
data_wide <- data_wide %>%
  mutate(change_mort_percent = -1*((1-(y2017/y2007))*100))


#====================================================================================================
#                                       Male
#====================================================================================================


str(data_wide)

# If all age groups should be included

# data_wide %>%
#   ggplot(aes(x = agegroup, y = region)) +
#   geom_tile(aes(fill = change_mort_percent)) +
#   scale_fill_gradient2(limits=c(-100, 100), breaks=seq(-100,100,by=25)) 

# Just include older age groups where differences are larger
data_wide_m_old <- data_wide %>% 
  filter(gender == "Male") %>%
  filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+")

# Graph male
mort_reg_m <- data_wide_m_old %>%
  ggplot(aes(x = agegroup, y = factor(region, level = c("TH", "ST", "SN", "SL", "SH", "RP", "NW", "NI", "MV", "HH", "HE", "HB", "BY", "BW", "BE", "BB")))) +
  geom_tile(aes(fill = change_mort)) +
  scale_fill_gradient2(limits=c(-600, 600), breaks=seq(-500,500,by=500)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8)) +
  labs(fill = "Mortality/100k") +
  ggtitle("Male")+
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))
  
  
mort_reg_m

#====================================================================================================
#                                      Female
#====================================================================================================

# Just include older age groups where differences are larger, censor data at -3000 for figure
data_wide_f_old <- data_wide %>% 
  filter(gender == "Female") %>%
  filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+")

# Graph female
mort_reg_f <- data_wide_f_old %>%
  ggplot(aes(x = agegroup, y = factor(region, level = c("TH", "ST", "SN", "SL", "SH", "RP", "NW", "NI", "MV", "HH", "HE", "HB", "BY", "BW", "BE", "BB")))) +
  geom_tile(aes(fill = change_mort)) + 
  scale_fill_gradient2(limits=c(-600, 600), breaks=seq(-500,500,by=500)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8))+
  labs(fill = "Mortality/100k") +
  ggtitle("Female") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5))


mort_reg_f

#====================================================================================================
#                                       cancer costs change
#====================================================================================================


# Reshape data to wide format

data_wide <- data %>% 
  subset(select = c(year, region, gender,agegroup, cancer_costs))

data_wide <- data_wide %>%
  pivot_wider(names_from = year, values_from = cancer_costs, names_prefix = "y")

data_wide <- data_wide %>%
  subset(select = c(region, gender,agegroup, y2007, y2017)) %>%
  filter(!is.na(y2007) | !is.na(y2017))


# data_wide_2007 <- data_wide %>%
#   subset(select = -c(y2017)) %>% 
#   filter(!is.na(y2007))
# data_wide_2017 <- data_wide %>%
#   subset(select = -c(y2007)) %>% 
#   filter(!is.na(y2017))
# 
# # Merge
# data_wide <- left_join(data_wide_2007, data_wide_2017, by = c("region", "gender", "agegroup"))

# Generate change in mortality per 100k from 2007 to 2017 per age, gender, region
data_wide <- data_wide %>%
  mutate(change_costs = y2017-y2007)

# Generate change in mortality in % (downside 0 observations are dropped)
data_wide <- data_wide %>%
  mutate(change_costs_percent = -1*((1-(y2017/y2007))*100))


#====================================================================================================
#                                       Male
#====================================================================================================

# If all age groups should be included

# data_wide %>%
#   ggplot(aes(x = agegroup, y = region)) +
#   geom_tile(aes(fill = change_mort_percent)) +
#   scale_fill_gradient2(limits=c(-100, 100), breaks=seq(-100,100,by=25)) 

# Just include older age groups where differences are larger
data_wide_m_old <- data_wide %>% 
  filter(gender == "Male") %>%
  filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+" )

# Graph male
costs_reg_m <- data_wide_m_old %>%
  ggplot(aes(x = agegroup, y = factor(region, level = c("TH", "ST", "SN", "SL", "SH", "RP", "NW", "NI", "MV", "HH", "HE", "HB", "BY", "BW", "BE", "BB")))) +
  geom_tile(aes(fill = change_costs)) +
  scale_fill_viridis(option = "inferno", direction = -1, limits=c(0, 2500), breaks=seq(0,2500,by=1000)) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8)) +
  labs(fill = "Costs per capita") +
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))

costs_reg_m

#====================================================================================================
#                                      Female
#====================================================================================================

# Just include older age groups where differences are larger, censor data at -3000 for figure
data_wide_f_old <- data_wide %>% 
  filter(gender == "Female") %>%
  filter(agegroup =="60-64" | agegroup == "65-69" | agegroup =="70-74" | agegroup =="75-79" | agegroup =="80-84" | agegroup =="85-89" | agegroup =="90+") #%>% 
#mutate(change_costs = replace(change_costs, change_costs < -3000, -3000))

# Graph female
costs_reg_f <- data_wide_f_old %>%
  ggplot(aes(x = agegroup, y = factor(region, level = c("TH", "ST", "SN", "SL", "SH", "RP", "NW", "NI", "MV", "HH", "HE", "HB", "BY", "BW", "BE", "BB")))) +
  geom_tile(aes(fill = change_costs)) + 
  scale_fill_viridis(option = "inferno", direction = -1, limits=c(0, 2500), breaks=seq(0,2500,by=1000)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 8))+
  labs(fill = "Costs per capita") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5))

costs_reg_f

# Combine graph

mort_reg_m + mort_reg_f + costs_reg_m + costs_reg_f +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")






