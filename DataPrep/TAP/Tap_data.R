install.packages("purrr")
library(purrr)

setwd("C:/Users/15712/Desktop/China_obs/Tap_data/xy")

data_files <- list.files("C:/Users/15712/Desktop/China_obs/Tap_data/xy")

t1 <- read.csv ("China_PM25_1km_2014_012.csv")
t2 <- read.csv("China_PM25_1km_2015_012.csv")
t3 <- read.csv("China_PM25_1km_2016_012.csv")

df_list <- list(t1, t2, t3)      

#merge all data frames together
tile_12 <- reduce(df_list, full_join, by='GridID')



tile_12 <- merge(t1, t2, t3, by = c("GridID"), all=TRUE)

#merging all years
#tap_2014a <- reduce(data_files, full_join, by='GridID')













x <- fread("urls.txt")

urls <- x %>% 
  sub('.htm$', '.zip', .) %>%    # change file suffix
  paste0('https://tap/mg/', .)    # append to base URL

# create a directory for it all
dir <- file.path(tempdir(), 'mg')
dir.create(dir)

# iterate and download
lapply(urls, 
       function(url) download.file(url, file.path(dir, basename(url)), method = "wget"))






# Specify URL where file is stored
url <- "http://minio.tapdata.org.cn:9000/tap-bj-1km/input_v3/Tile_012_lonlat.csv"
# Specify destination where file should be saved
destfile <- "C:/Users/15712/Desktop/China_obs/Tap_data/tap1.csv"


# Apply download.file function in R
download.file(url, destfile)


