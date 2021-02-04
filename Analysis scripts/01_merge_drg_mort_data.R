library(bannerCommenter)
library(tidyverse)
library(dplyr)
#library(stats)
library(collapse)

############################################################################
############################################################################
###                                                                      ###
###                                STEP X:                               ###
###                  Prepare DRG data for merge                          ###
###                                                                      ###
############################################################################
############################################################################

# Extract information (try using with loops 2007 to 2009 identical also the rest)
# https://www.r-bloggers.com/2014/01/looping-through-files/
  
path <- "../../Data collection/Auswertung FDZ"
setwd("../../Data collection/Auswertung FDZ")

file.names <- dir(path, pattern = ".log")
file.names



##################################################################
##                                                              ##
##            Extract total costs and N                         ##
##################################################################


out.file1 <- ""

for(i in 1:3){
  file <- read.table(file.names[i], skip = 33, nrows = 672)
  file['year'] = i # add time period, 1 = 2007, 11 = 2017
  out.file1 <- rbind(out.file1, file)
}

for(i in 4:11){
  file <- read.table(file.names[i], skip = 34, nrows = 672)
  file['year'] = i # add time period, 1 = 2007, 11 = 2017
  out.file1 <- rbind(out.file1, file)
}

# Drop v2 and rename v3 and v4
out.file1 <- out.file1 %>%
  slice(-1)  %>%
  mutate(ID = row_number(), group = V1, tot_costs = V3, tot_N =V4) %>% # add ID using row_number() in dplyr
  select(-V1, -V2, -V3, -V4)  


##################################################################
##                                                              ##
##              Extract CVD costs and N                         ##
##################################################################


out.file2 <- ""

for(i in 1:3){
  file <- read.table(file.names[i], skip = 714, nrows = 672)
  file['year'] = i # add time period, 1 = 2007, 11 = 2017
    out.file2 <- rbind(out.file2, file)
}

for(i in 4:11){
  file <- read.table(file.names[i], skip = 715, nrows = 672)
  file['year'] = i # add time period, 1 = 2007, 11 = 2017
  out.file2 <- rbind(out.file2, file)
}

# Drop v2 and rename v3 and v4
out.file2 <- out.file2 %>%
  slice(-1)  %>%
  mutate(ID = row_number(), group = V1, CVD_costs = V3, CVD_N =V4) %>%
  select(-V1, -V2, -V3, -V4)  


##################################################################
##                                                              ##
##           Extract cancer costs and N                         ##
##################################################################

out.file3 <- ""

for(i in 1:3){
  file <- read.table(file.names[i], skip = 1395, nrows = 672)
  file['year'] = i # add time period, 1 = 2007, 11 = 2017
  out.file3 <- rbind(out.file3, file)
}

for(i in 4:11){
  file <- read.table(file.names[i], skip = 1396, nrows = 672)
  file['year'] = i # add time period, 1 = 2007, 11 = 2017
  out.file3 <- rbind(out.file3, file)
}

# Drop v2 and rename v3 and v4
out.file3 <- out.file3 %>%
  slice(-1)  %>%
  mutate(ID = row_number(), group = V1, cancer_costs = V3, cancer_N =V4) %>%
  select(-V1, -V2, -V3, -V4)  

##################################################################
##                                                              ##
##             Extract resp costs and N                         ##
##################################################################

out.file4 <- ""

for(i in 1:3){
  file <- read.table(file.names[i], skip = 2076, nrows = 672)
  file['year'] = i # add time period, 1 = 2007, 11 = 2017
  out.file4 <- rbind(out.file4, file)
}
  
for(i in 4:11){
  file <- read.table(file.names[i], skip = 2077, nrows = 672)
  file['year'] = i # add time period, 1 = 2007, 11 = 2017
  out.file4 <- rbind(out.file4, file)
}

# Drop v2 and rename v3 and v4
out.file4 <- out.file4 %>%
  slice(-1)  %>%
  mutate(ID = row_number(), group = V1, resp_costs = V3, resp_N =V4) %>%
  select(-V1, -V2, -V3, -V4)  


##################################################################
##                                                              ##
##             Merge tables to full cost table                  ##
##################################################################

# Merge
full_cost_table <- out.file1 %>%
  left_join(out.file2, by = "ID") %>% 
  left_join(out.file3, by = "ID") %>% 
  left_join(out.file4, by = "ID")

# Only keep variables needed
full_cost_table <- full_cost_table %>%
  select(-year.y, -year.x.x, -year.y.y, -group.y, - group.x.x, - group.y.y,) %>% 
  mutate(period = year.x, group = group.x) %>% 
  select(-year.x, -group.x)

# Recode as numeric
full_cost_table <- full_cost_table %>%
  mutate(tot_costs = as.numeric(tot_costs),
         CVD_costs = as.numeric(CVD_costs),
         cancer_costs = as.numeric(cancer_costs),
         resp_costs = as.numeric(resp_costs),
         tot_N = as.numeric(tot_N),
         CVD_N = as.numeric(CVD_N),
         cancer_N = as.numeric(cancer_N),
         resp_N = as.numeric(resp_N)
         )

# Count missings due to anonymisation

ind <- is.na(full_cost_table$CVD_costs) # logical TRUE/FALSE vector
sum(ind) # countrs the ones for which this is TRUE

ind <- is.na(full_cost_table$CVD_N) # logical TRUE/FALSE vector
sum(ind) # countrs the ones for which this is TRUE

# Missings in costs: 0, 10, 167,0
# Missings in N: 0, 9, 159,0



sum(full_cost_table$tot_costs)

class(full_cost_table$tot_N)
head(full_cost_table)
class(full_cost_table)

##################################################################
##                                                              ##
##        Combine age groups to match with mortality data       ##
##################################################################

# combine age groups 2 3 and 4
str(full_cost_table)

# Total
agegroup_2_tot <- full_cost_table %>% 
  filter(str_detect(group, "02$") | str_detect(group, "03$") | str_detect(group,"04$")) %>% # ends with 04 add dollar
  separate(group, into = c("reg_s", "age"), sep = 3) %>% # separate group first 3 digits as separator
  unite("year_group", c("period", "reg_s")) %>% 
  mutate(tot_tot_costs = tot_costs*tot_N) %>% 
  group_by(year_group) %>%
  select(year_group, tot_tot_costs, tot_N) %>% 
summarise(tot_costs = sum(tot_tot_costs)/sum(tot_N), tot_N = sum(tot_N))

# CVD
agegroup_2_CVD <- full_cost_table %>% 
  filter(str_detect(group, "02$") | str_detect(group, "03$") | str_detect(group,"04$")) %>% # ends with 04 add dollar
  separate(group, into = c("reg_s", "age"), sep = 3) %>% # separate group first 3 digits as separator
  unite("year_group", c("period", "reg_s")) %>% 
  mutate(tot_CVD_costs = CVD_costs*CVD_N) %>% 
  group_by(year_group) %>%
  select(year_group, tot_CVD_costs, CVD_N) %>% 
  summarise(CVD_costs = sum(tot_CVD_costs)/sum(CVD_N), CVD_N = sum(CVD_N))

# Cancer
agegroup_2_cancer <- full_cost_table %>% 
  filter(str_detect(group, "02$") | str_detect(group, "03$") | str_detect(group,"04$")) %>% # ends with 04 add dollar
  separate(group, into = c("reg_s", "age"), sep = 3) %>% # separate group first 3 digits as separator
  unite("year_group", c("period", "reg_s")) %>% 
  mutate(tot_cancer_costs = cancer_costs*cancer_N) %>% 
  group_by(year_group) %>%
  select(year_group, tot_cancer_costs, cancer_N) %>% 
  summarise(cancer_costs = sum(tot_cancer_costs)/sum(cancer_N), cancer_N = sum(cancer_N))

# resp
agegroup_2_resp <- full_cost_table %>% 
  filter(str_detect(group, "02$") | str_detect(group, "03$") | str_detect(group,"04$")) %>% # ends with 04 add dollar
  separate(group, into = c("reg_s", "age"), sep = 3) %>% # separate group first 3 digits as separator
  unite("year_group", c("period", "reg_s")) %>% 
  mutate(tot_resp_costs = resp_costs*resp_N) %>% 
  group_by(year_group) %>%
  select(year_group, tot_resp_costs, resp_N) %>% 
  summarise(resp_costs = sum(tot_resp_costs)/sum(resp_N), resp_N = sum(resp_N))

# Merge subtables
age2_comb <- agegroup_2_tot %>%
  left_join(agegroup_2_CVD) %>%
  left_join(agegroup_2_cancer) %>%
  left_join(agegroup_2_resp) %>% 
  mutate(agegroup = 2)


# combine age groups 20 and 21

# Total
agegroup_18_tot <- full_cost_table %>% 
  filter(str_detect(group, "20$") | str_detect(group, "21$")) %>% # ends with 04 add dollar
  separate(group, into = c("reg_s", "age"), sep = 3) %>% # separate group first 3 digits as separator
  unite("year_group", c("period", "reg_s")) %>% 
  mutate(tot_tot_costs = tot_costs*tot_N) %>% 
  group_by(year_group) %>%
  select(year_group, tot_tot_costs, tot_N) %>% 
  summarise(tot_costs = sum(tot_tot_costs)/sum(tot_N), tot_N = sum(tot_N))

# CVD
agegroup_18_CVD <- full_cost_table %>% 
  filter(str_detect(group, "20$") | str_detect(group, "21$")) %>% # ends with 04 add dollar
  separate(group, into = c("reg_s", "age"), sep = 3) %>% # separate group first 3 digits as separator
  unite("year_group", c("period", "reg_s")) %>% 
  mutate(tot_CVD_costs = CVD_costs*CVD_N) %>% 
  group_by(year_group) %>%
  select(year_group, tot_CVD_costs, CVD_N) %>% 
  summarise(CVD_costs = sum(tot_CVD_costs)/sum(CVD_N), CVD_N = sum(CVD_N))

# Cancer
agegroup_18_cancer <- full_cost_table %>% 
  filter(str_detect(group, "20$") | str_detect(group, "21$")) %>% # ends with 04 add dollar
  separate(group, into = c("reg_s", "age"), sep = 3) %>% # separate group first 3 digits as separator
  unite("year_group", c("period", "reg_s")) %>% 
  mutate(tot_cancer_costs = cancer_costs*cancer_N) %>% 
  group_by(year_group) %>%
  select(year_group, tot_cancer_costs, cancer_N) %>% 
  summarise(cancer_costs = sum(tot_cancer_costs)/sum(cancer_N), cancer_N = sum(cancer_N))

# resp
agegroup_18_resp <- full_cost_table %>% 
  filter(str_detect(group, "20$") | str_detect(group, "21$")) %>% # ends with 04 add dollar
  separate(group, into = c("reg_s", "age"), sep = 3) %>% # separate group first 3 digits as separator
  unite("year_group", c("period", "reg_s")) %>% 
  mutate(tot_resp_costs = resp_costs*resp_N) %>% 
  group_by(year_group) %>%
  select(year_group, tot_resp_costs, resp_N) %>% 
  summarise(resp_costs = sum(tot_resp_costs)/sum(resp_N), resp_N = sum(resp_N))

# Merge subtables
age18_comb <- agegroup_18_tot %>%
  left_join(agegroup_18_CVD) %>%
  left_join(agegroup_18_cancer) %>%
  left_join(agegroup_18_resp) %>% 
  mutate(agegroup = 18)

# Append age group tables together
age_table <- bind_rows(age2_comb, age18_comb)

##################################################################
##                                                              ##
##        Generate group variables for merge with mortality     ##
##################################################################

# Age table
age_table_inb <- age_table %>%
  separate(year_group, into = c("period", "reggen"), sep = 2) %>% 
  mutate(period = str_replace_all(period, "_", ""),
         reggen = str_replace_all(reggen, "_", ""),
         ) %>% 
  separate(reggen, into = c("region", "gender"), sep = 2) 

# Full table
full_cost_table_inb <- full_cost_table %>%
  separate(group, into = c("region", "genage"), sep = 2) %>% 
  separate(genage, into = c("gender", "agegroup"), sep = 1) %>% 
  filter(agegroup != "02" & agegroup !="03" & agegroup !="04" & agegroup !="20" & agegroup !="21") %>% # drop age groups
  mutate(agegroup = recode(agegroup, '01' = "1", 
                           '05' = "3",
                           '06' = "4",
                           '07' = "5",
                           '08' = "6",
                           '09' = "7",
                           '10' = "8",
                           '11' = "9",
                           '12' = "10",
                           '13' = "11",
                           '14' = "12",
                           '15' = "13",
                           '16' = "14",
                           '17' = "15",
                           '18' = "16",
                           '19' = "17")
  ) %>% 
  select(-ID) %>% 
  mutate(agegroup = as.numeric(agegroup))

##################################################################
##                                                              ##
##             Merge full cost and age group table             ##
##################################################################

cost_data <- bind_rows(full_cost_table_inb, age_table_inb)

# Numerical identifiers
cost_data <- cost_data %>%
  mutate(region = recode(region, '01' = "1", 
                           '02' = "2",
                           '03' = "3",
                           '04' = "4",
                           '05' = "5",
                           '06' = "6",
                           '07' = "7",
                           '08' = "8",
                           '09' = "9")
  ) %>% 
  mutate(region = as.numeric(region), period = as.numeric(period)) %>% 
  mutate(gender = recode(gender, 'm' = "1", 'w' = "2"), gender = as.numeric(gender))

cost_data <- as_tibble(cost_data)
str(cost_data)

# Set working directory back to project directory
setwd("../../Analysis Paper 7/test6")




##################################################################
##                                                              ##
##             Prepare mortality table for merge              ##
##################################################################

# Mortality data
full_mortality_table <- read_csv("../../Data collection/Todesursachenstatistik/Dataset Mortality.csv")
full_mortality_table <- full_mortality_table %>% filter(year >= 2007 & year !=2018) # drop years without cost information


# Generate group variables for merge
full_mortality_table_inb <- full_mortality_table %>% 
  mutate(region = recode(region, "Baden-Wurttemberg"="8",
                         "Bayern" = "9",
                         "Berlin" = "11",
                         "Brandenburg" = "12",
                         "Bremen" = "4",
                         "Hamburg" = "2",
                         "Hessen" = "6",
                         "Mecklenburg-Vorpommern" = "13",
                         "Niedersachsen" = "3",
                         "Nordrhein-Westfalen" = "5",
                         "Rheinland-Pfalz" = "7",
                         "Saarland" = "10",
                         "Sachsen" = "14",
                         "Sachsen-Anhalt" = "15",
                         "Schleswig-Holstein" = "1",
                         "Thuringen" = "16")) %>% 
  mutate(region = as.numeric(region)) %>% 
  mutate(agegroup = recode(agegroup, 'Unter 1 Jahr' = "1",
                           '1 Jahr bis unter 15 Jahre' = "2",
                           '15 bis unter 20 Jahre' = "3",
                           '20 bis unter 25 Jahre' = "4",
                           '25 bis unter 30 Jahre' = "5",
                           '30 bis unter 35 Jahre' = "6",
                           '35 bis unter 40 Jahre' = "7",
                           '40 bis unter 45 Jahre' = "8",
                           '45 bis unter 50 Jahre' = "9",
                           '50 bis unter 55 Jahre' = "10",
                           '55 bis unter 60 Jahre' = "11",
                           '60 bis unter 65 Jahre' = "12",
                           '65 bis unter 70 Jahre' = "13",
                           '70 bis unter 75 Jahre' = "14",
                           '75 bis unter 80 Jahre' = "15",
                           '80 bis unter 85 Jahre' = "16",
                           '85 bis unter 90 Jahre' = "17",
                           '90 Jahre und alter' = "18")) %>% 
  mutate(agegroup = as.numeric(agegroup)) %>% 
  mutate(gender = recode(gender, "Mannlich" = "1", "Weiblich" = "2")) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(period = recode(year, "2007" = "1",
                         "2008" ="2",
                         "2009" ="3",
                         "2010" ="4",
                         "2011" ="5",
                         "2012" ="6",
                         "2013" ="7",
                         "2014" ="8",
                         "2015" ="9",
                         "2016" ="10",
                         "2017" ="11")) %>% 
  mutate(period = as.numeric(period)) %>% 
  select(-year)



  
mortality_data <- full_mortality_table_inb


##################################################################
##                                                              ##
##             Merge mortality and cost tables              ##
##################################################################

str(mortality_data)
str(cost_data)

# Merge
str(data)
data <- mortality_data %>% left_join(cost_data) %>% arrange(period, region, gender, agegroup) # sort columns

data <- data[,c(13,1:12,14:21)] # order columns

save(data, file = "./Data_ready/full_analysis_data.rda")
