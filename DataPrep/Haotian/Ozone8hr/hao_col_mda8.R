rm( list = ls())

library(pryr)
library(fst)
library( data.table)
library( parallel)
library(dplyr)
library(openair)
library(lubridate)


filepath_dir <- "D:/Professional/Projects/China_obs/Haotian/home/jyq/aqdata"

# select the correct year directory, then list all the files
files <- list.files( filepath_dir,
                     full.names = TRUE,
                     recursive = TRUE)

# 2012 has a different format, so remove
files_no_2012 <- files[!grepl('aqdata/2012/', files)]

# write a function to read them in
reader.fn <- function( file_in ){
  # select the year, month, and station ID from the file names
  # 
  # gsub replaces all regex matches with the second argument ('')
  
  # matches anything (.*) followed by slash (.*/) OR (|) ".txt"
  sta.ID <- gsub( '.*/|.txt', '', file_in)  
  
  # matches anything followed by aqdata/ (.*aqdata/) OR 
  #     any number of digits folowed by a slash follwed by anything (\\d+/.*)
  yr <- gsub( '.*aqdata/|/\\d+/.*', '', file_in)
  
  # matches anything followed by aqdata/ (.*aqdata/) followed by any number of digits and a slash (.*aqdata/\\d+/) OR 
  #     a slash followed by any number of digits folowed by A (/\\d+A.txt)
  mon <- gsub( '.*aqdata/\\d+/|/\\d+A.txt', '', file_in)
  
  # read the file
  dat_in <- fread( file_in)
  
  if (ncol(dat_in) == 7)
  {
    
    # assign the names
    names( dat_in) <- c( "day_hr", "NO2","SO2","O3","PM2.5","PM10","CO")
    
    # replace missing with NA
    dat_in[ dat_in == -999] <- NA
    
    # convert day_hr to string with padded zero, then split
    dat_in[, day_hr := formatC( day_hr, width = 10, flag = '0')]
    dat_in[, `:=`( day = substr( day_hr, 7, 8),
                   hr = substr( day_hr, 9, 10))]
    
    # summarize by day, because I think the size of the hourly data is prohibitively large
    # first, melt it for easier processing
    dat_in.m <- melt( dat_in[, .( day, hr, NO2, SO2, O3, PM2.5, PM10)], 
                      id.vars = c( 'day', 'hr'))
    
    #subseting to just ozone 
     dat_in_o3 <- subset(dat_in.m, variable == "O3")
    
  
    # take average, also calculate count of non-missing
    dat_day <- 
      dat_in_o3[ !is.na( value), 
                .( hr_mean = mean( value),
                   station = sta.ID,
                   N = .N),
                by = .( day, hr, variable)]
    
    
    # create output data table
    dat_day[, date := make_datetime( year = yr, month = mon, day = day,
                                       hour = hr)]
    
    #calculate rolling mean
    dat_day <- rollingMean(dat_day, pollutant = "hr_mean", width = 8,
                          new.name = "rollingo3", data.thresh = 75)
    
    # return dataset
    return( dat_day[,.( station, date, hr, variable, rollingo3, N)])
    
  }
  
  else
    return ("x = 4")
}

# mclapply will use all cores on your computer
# rbind the list
in_data_daily <- 
  mclapply( files_no_2012[1:500],
            reader.fn) %>% rbindlist()

#save datasets
write.csv(in_data_daily, "D:/Professional/Projects/China_obs/Haotian/Haotian_ozone_roll.csv", row.names = FALSE)

#read data
in_data_daily <- read.fst("D:/Professional/Projects/China_obs/Haotian/Haotian_ozone_hourly.fst")
summary(in_data_daily)

#calculate rolling mean
mydata <- rollmean(in_data_daily, 8, na.pad = TRUE, 
                   align = c("center"))

