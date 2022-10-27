##Annual Mean##
library( data.table)
library(dplyr)
library( magrittr) 

#set working directory same as the files
setwd("C:/Users/15712/Desktop/China_obs/Obs_annual/Plot")

#read files
values <- read.csv("china_mean_14_22.csv")
station <- read.csv("stations.csv")

#delete the extra 'x's in the station id's
values$station.name = gsub('X', '',  values$station.name)

#rename some columns
station <- station %>%
  rename(station.name = ï..station.name)

#merge datasets
annual_data <- merge(values, station, by = "station.name")

#save datasets
fwrite(annual_data, file = "PM_annual_14_22.csv", row.names = F)

