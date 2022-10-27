#Year 2017: Observational Data
#Data Preparation
#From Feb: new stations added
#set working directory same as the files
setwd("C:/Users/15712/Desktop/China_obs/??????_20170101-20171231")

#list the files
data_files <- list.files("C:/Users/15712/Desktop/China_obs/??????_20170101-20171231")

#Loop over file list importing them and binding them together
data_2017 <- 
  lapply( data_files, fread, header = TRUE, stringsAsFactors = FALSE) %>%
  rbindlist(fill=TRUE)

#keep data only for PM2.5
PM_2017 <- subset(data_2017, data_2017$type == "PM2.5")

# melt to long format for easier averaging
melt_var_names <- c( 'date', 'hour', 'type')
station_names <- names( PM_2017)[ !(names( PM_2017) %in% melt_var_names)]
PM_2017.m <- melt( PM_2017, 
                   id.vars = c( 'date', 'hour'),
                   measure.vars = station_names,
                   variable.name = 'station.name',
                   value.name = 'PM2.5')

#calculate mean for all columns
PM_2017_mean <- PM_2017.m[, .( PM = mean( PM2.5, na.rm = TRUE),
                               n.NA = sum( is.na( PM2.5))), 
                          by = station.name]

# add a year column
PM_2017_mean[, year := 2017]


#save datasets
write.csv(PM_2017, file = "PM_2017_hourly.csv", row.names = F)
write.csv(PM_2017_mean, file = "PM_2017_annual_mean.csv", row.names = F)

