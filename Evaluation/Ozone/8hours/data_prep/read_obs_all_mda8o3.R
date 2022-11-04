rm( list = ls())

library(pryr)
library(fst)
library( data.table)
library( parallel)
library(dplyr)
library( lubridate)
library( openair)

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
  # gsub replaces all regex matches with the second argument ('')
  
  # matches anything (.*) followed by slash (.*/) OR (|) ".txt"
  sta.ID <- gsub( '.*/|.txt', '', file_in)  
  
  # matches anything followed by aqdata/ (.*aqdata/) OR 
  #     any number of digits folowed by a slash follwed by anything (\\d+/.*)
  yr <- gsub( '.*aqdata/|/\\d+/.*', '', file_in)
  
  # matches anything followed by aqdata/ (.*aqdata/) followed by any number of digits and a slash (.*aqdata/\\d+/) OR 
  #     a slash followed by any number of digits folowed by A (/\\d+A.txt)
  mon <- gsub( '.*aqdata/\\d+/|/\\d+A.txt', '', file_in)
  
  # get the previous month
  dat_prev_month <- as.Date( paste(yr, mon, '01', sep = '-')) - 1
  day_prev <- mday( dat_prev_month)
  mon_prev <- month( dat_prev_month) 
  yr_prev <- year( dat_prev_month) 
  
  # create the previous month file's name
  file_prev <- 
    gsub( paste0( '/aqdata/', yr, '/'),
          paste0( '/aqdata/', yr_prev, '/'), file_in)
  file_prev <- 
    gsub( paste0( '/', mon, '/', sta.ID),
          paste0( '/', mon_prev, '/', sta.ID), file_prev)
  
  # read the file
  dat_in <- fread( file_in)
  
  # read second file if it exists
  if( file.exists( file_prev)){
    dat_in_prev <- fread( file_prev) 
  } else
    dat_in_prev <- dat_in[0]
  
  # check for number of columns
  if (ncol(dat_in) == 7){
    
    # assign the names
    names( dat_in) <- c( "day_hr", "NO2","SO2","O3","PM2.5","PM10","CO")
    names( dat_in_prev) <- c( "day_hr", "NO2","SO2","O3","PM2.5","PM10","CO")
    
    # replace missing with NA
    dat_in[ dat_in == -999] <- NA
    dat_in_prev[ dat_in_prev == -999] <- NA
    
    # convert day_hr to string with padded zero, then split
    dat_in[, day_hr := formatC( day_hr, width = 10, flag = '0')]
    dat_in_prev[, day_hr := formatC( day_hr, width = 10, flag = '0')]
    
    dat_in[, `:=`( mon.m = mon,
                   day = substr( day_hr, 7, 8),
                   hr = substr( day_hr, 9, 10))]
    dat_in_prev[, `:=`( mon.m = mon_prev,
                        day = substr( day_hr, 7, 8),
                        hr = substr( day_hr, 9, 10))]
    
    # rbind with previous month's final day
    if( nrow( dat_in_prev) > 0){
      dat_in_add_prev <- 
        rbindlist( 
          list( 
            dat_in_prev[day == day_prev],
            dat_in
          )
        )
    } else
      dat_in_add_prev <- dat_in
    
    # summarize by day, because I think the size of the hourly data is prohibitively large
    # first, melt it for easier processing
    dat_in.m <- melt( dat_in_add_prev[, .( mon.m, day, hr, NO2, SO2, O3, PM2.5, PM10)], 
                      id.vars = c( 'mon.m', 'day', 'hr'))
    
    #subseting to just ozone 
    dat_in_o3 <- subset(dat_in.m, variable == "O3")
    
    # create output data table
    dat_in_o3[, 
              `:=` ( date = make_datetime( 
                year = yr, month = mon.m, 
                day = day, hour = hr),
                year = yr,
                station = sta.ID)]
    
    # calculate rolling averages
    dat_rollmeans <- 
      rollingMean( dat_in_o3, pollutant = 'value', width = 8,
                   new.name = 'rollingo3', data.thresh = 75, align = 'right')
    
    # calculate maximums by day
    dat_daymaxes <- 
      dat_rollmeans[ mon.m == mon,
                     .( mda_o3 = max( rollingo3)),
                     by = .( year, mon.m, day, station)]
    dat_daymaxes[, date := paste( year, mon.m, day, sep = '-')]
    
    # return dataset
    return( dat_daymaxes)
    
  } else{
    message( paste( 'Check file', file_in, '- inconsistent format'))
    dat_daymaxes2 <- data.table()
    return ( dat_daymaxes)
  }
}

# mclapply will use all cores on your computer
# rbind the list
in_data_daily <- 
  mclapply( files_no_2012[1:122583],
            reader.fn) %>% rbindlist()

#save datasets
write.fst(in_data_daily, "D:/Professional/Projects/China_obs/Haotian/Haotian_ozone_mda8h.fst")

#read data
in_data_daily <- read.fst("D:/Professional/Projects/China_obs/Haotian/Haotian_ozone_mda8h.fst")
summary(in_data_daily)

