#load raster into R
huang_14 <- raster("PM25_2008.tif") 

## convert to a df; first, to a SpatialPointsDataFrame
huang14_pts <- rasterToPoints(huang_14, spatial = TRUE)

#convert to a conventional  dataframe
huang_df  <- data.frame(huang14_pts)

# merge with grid 
huang_grid <- cbind( grid[, .( X, Y)], huang_df) 

#save datasets
fwrite(huang_grid, file = "PM_2008_Huang.csv")

setwd("C:/Users/15712/Desktop/China_obs/Huang/PM25AnnualMean2008-2019/data")

#list the files
data_files <- list.files("C:/Users/15712/Desktop/China_obs/Huang/PM25AnnualMean2008-2019/data")

#Loop over file list importing them and binding them together
data_annual <- 
  lapply( data_files, fread, header = TRUE, stringsAsFactors = FALSE) %>%
  rbindlist(fill=TRUE)

#save data
fwrite(data_annual, file = "huang_08_19.csv", row.names = F)


setwd("C:/Users/15712/Desktop/China_obs/All_Data_PM2.5")
li <- read.csv("Tiantian_from_annual_11_17.csv")

# merge the two 
pm_in <- cbind( grid[, .( X, Y)], li) 

fwrite(pm_in, file = "li_grid.csv", row.names = F)











