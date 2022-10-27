#Year 2015: Observational Data
#Data Preparation
#set working directory same as the files
setwd("C:/Users/15712/Desktop/China_obs/??????_20150101-20151231")

#list the files
data_files <- list.files("C:/Users/15712/Desktop/China_obs/??????_20150101-20151231")

#Loop over file list importing them and binding them together
data_2015 <- do.call("rbind",lapply(data_files, read.csv, header = TRUE, stringsAsFactors = FALSE))

#keep data only for PM2.5
PM_2015 <- subset(data_2015, data_2015$type == "PM2.5")

#replace NAs with zero
PM_2015[is.na(PM_2015)] = 0

#calculate mean for all columns
PM_2015_mean <- as.data.frame(colMeans(PM_2015[sapply(PM_2015, is.numeric)]))

#save datasets
write.csv(PM_2015, file = "PM_2015_hourly_station_id.csv", row.names = F)
write.csv(PM_2015_mean, file = "PM_2015_annual_mean_station_id.csv", row.names = T)

