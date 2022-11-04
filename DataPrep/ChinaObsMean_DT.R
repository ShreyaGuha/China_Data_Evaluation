#Year 2015: Observational Data ——edited by LH with ideas for using data.table

library( data.table)
library( magrittr) # allows us to use the %>% operator
#Data Preparation
#set working directory same as the files
setwd("C:/Users/15712/Desktop/China_obs/??????_20150101-20151231")

#list the files (just changing to my files so I can test)
# data_files <- list.files("C:/Users/15712/Desktop/China_obs/??????_20150101-20151231")
data_files <- list.files("~//Library/CloudStorage/OneDrive-SharedLibraries-GeorgeMasonUniversity-O365Production/HAQ\ Lab-GRP\ -\ Documents/People/GuhaShreya/AQ_fields_china/air\ data/城市_20150101-20141231",
                         full.names = TRUE)

#Loop over file list importing them and binding them together
# data_2015 <- do.call("rbind",lapply(data_files, read.csv, header = TRUE, stringsAsFactors = FALSE))
data_2015 <- 
  lapply( data_files, fread, header = TRUE, stringsAsFactors = FALSE) %>%
  rbindlist
dim( data_2015) # 123165    370


#keep data only for PM2.5
PM_2015 <- data_2015[ type == "PM2.5"]
dim( PM_2015) # 8211  370

# melt to long format for easier averaging
melt_var_names <- c( 'date', 'hour', 'type')
station_names <- names( PM_2015)[ !(names( PM_2015) %in% melt_var_names)]
PM_2015.m <- melt( PM_2015, 
                   id.vars = c( 'date', 'hour'),
                   measure.vars = station_names,
                   variable.name = 'station.name',
                   value.name = 'PM2.5')

#calculate mean for all columns
PM_2015_mean <- PM_2015.m[, .( PM = mean( PM2.5, na.rm = TRUE),
                               n.NA = sum( is.na( PM2.5))), 
                          by = station.name]

# add a year column
PM_2015_mean[, year := 2015]

#save datasets
fwrite(PM_2015, file = "PM_2015_hourly.csv", row.names = F)
fwrite(PM_2015_mean, file = "PM_2015_annual_mean.csv", row.names = F)








