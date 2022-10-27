#set working directory
setwd("C:/Users/15712/Desktop/China_obs/Tap_data/tap")

#list downloaded files
data_files <- list.files("C:/Users/15712/Desktop/China_obs/Tap_data/tap")

#read files
data_files <- lapply(data_files, read.csv) 

#merge all data files together
tap_2014a <- bind_rows(data_files)

#save dataset
fwrite(tap_2014a, file = "2014.csv", row.names = F)


#repeat for each year to get yearwise data and 
#for all the tiles to get coordinate info

#read each year info
t14 <- read.csv("tap_2014.csv")
t15 <- read.csv("tap_2015.csv")
t16 <- read.csv("tap_2016.csv")
t17 <- read.csv("tap_2017.csv")
t18 <- read.csv("tap_2018.csv")
t19 <- read.csv("tap_2019.csv")
t20 <- read.csv("tap_2020.csv")
t21 <- read.csv("tap_2021.csv")

#enter column concerning year info
#should have done this earlier
t14$year <- c(2014)
t15$year <- c(2015)
t16$year <- c(2016)
t17$year <- c(2017)
t18$year <- c(2018)
t19$year <- c(2019)
t20$year <- c(2020)
t21$year <- c(2021)

#set memory limit, this is very crucial
memory.limit(size = 1200000000)

#open merged file containing lat-lon info
coord <- read.csv("coord.csv")

#merge with yearly dataset
t14 <- merge(t14, coord, by = c("GridID"), all=TRUE)
t15 <- merge(t15, coord, by = c("GridID"), all=TRUE)
t16 <- merge(t16, coord, by = c("GridID"), all=TRUE)
t17 <- merge(t17, coord, by = c("GridID"), all=TRUE)
t18 <- merge(t18, coord, by = c("GridID"), all=TRUE)
t19 <- merge(t19, coord, by = c("GridID"), all=TRUE)
t20 <- merge(t20, coord, by = c("GridID"), all=TRUE)
t21 <- merge(t21, coord, by = c("GridID"), all=TRUE)

#save final dataset
fwrite(t14, file = "tap_2014.csv", row.names = F)
fwrite(t15, file = "tap_2015.csv", row.names = F)
fwrite(t16, file = "tap_2016.csv", row.names = F)
fwrite(t17, file = "tap_2017.csv", row.names = F)
fwrite(t18, file = "tap_2018.csv", row.names = F)
fwrite(t19, file = "tap_2019.csv", row.names = F)
fwrite(t20, file = "tap_2020.csv", row.names = F)
fwrite(t21, file = "tap_2021.csv", row.names = F)


