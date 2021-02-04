# In this file, write the R-code necessary to load your original data file
# (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# use the function open_data(your_data_frame) or closed_data(your_data_frame)
# to store the data.

library(worcs)
library(tidyverse)



load("./Data_ready/clean_data.rda")
open_data(data)


load("./Data_ready/shapefile.rda")
open_data(shp_df)
