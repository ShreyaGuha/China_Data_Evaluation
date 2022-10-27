#Year 2018: Observational Data
#Data Preparation
#From Feb: new stations added
#set working directory same as the files
setwd("C:/Users/15712/Desktop/China_obs/??????_20180101-20181231")

#list the files
data_files <- list.files("C:/Users/15712/Desktop/China_obs/??????_20180101-20181231")

#Loop over file list importing them and binding them together
data_2018 <- 
  lapply( data_files, fread, header = TRUE, stringsAsFactors = FALSE) %>%
  rbindlist(fill=TRUE)

#keep data only for PM2.5
PM_2018 <- subset(data_2018, data_2018$type == "PM2.5")

#replace NAs with zero
PM_2018[is.na(PM_2018)] = 0


#save datasets
write.csv(PM_2018, file = "PM_2018_hourly.csv", row.names = F)
#1 column name <html> with zero values omitted

#open edited dataset
PM_2018 <- read.csv("PM_2018_hourly.csv")

#calculate mean for all columns
PM_2018_mean <- as.data.frame(colMeans(PM_2018[sapply(PM_2018, is.numeric)]))

#save datasets
write.csv(PM_2018_mean, file = "PM_2018_annual_mean.csv", row.names = T)

