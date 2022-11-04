library( sf)
library(sp)
library( raster)
library( data.table)
library( ggplot2)
library( viridis)
library( pbmcapply)
library( dplyr)
library(ncdf4)

##Initial Preparation##

setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents")

#read the grid files
grid <- fread("grid.csv")

china.shp <- 'bou2_4p.shp'
china.shp <- st_read(china.shp, crs = 'WGS84')

##to interpret the province name and categorize##

china.shp.dt <- data.table::setDT(china.shp)
data.table::setnames(china.shp.dt, "NAME", "province")

# transform to UTF-8 coding format
china.shp.dt[, province:=iconv(province, from = "GBK", to = "UTF-8")] 

# add the province EN, CH label file
china.shp.dt[, province:= as.factor(province)]
province_CH<-china.shp.dt[, levels(province)] # the CH are in UTF-8 code
province_EN <- c("Shanghai", "Yunnan", "Inner Mongolia", "Beijing", "Taiwan",
                 "Jilin", "Sichuan", "Tianjin", "Ningxia", "Anhui",
                 "Shandong", "Shanxi", "Guangdong", "Guangxi", "Xinjiang",
                 "Jiangsu", "Jiangxi", "Hebei", "Henan", "Zhejiang",
                 "Hainan", "Hubei", "Hunan", "Gansu", "Fujian",
                 "Tibet", "Guizhou", "Liaoning", "Chongqing", "Shaanxi",
                 "Qinghai", "Hong Kong", "Heilongjiang")


# make a data frame of Chinese and English name for provinces
province_name<-data.frame(province=province_CH,
                          province_english=province_EN)

# merge with china.shp
china.shp.reg <- left_join(china.shp.dt,province_name,by="province")

# categorize regions in China(common practice)
china.shp.reg.sf <-
  china.shp.reg %>% 
  mutate(region=case_when(province_english %in% c("Shanghai","Jiangsu","Zhejiang","Anhui",
                                                  "Shandong","Fujian","Jiangxi","Taiwan") ~ "East",
                          province_english %in% c("Beijing","Tianjin","Hebei","Shanxi","Inner Mongolia") ~ "North",
                          province_english %in% c("Henan","Hubei","Hunan") ~ "Central",
                          province_english %in% c("Guangdong","Guangxi","Hainan","Hong Kong") ~ "South",
                          province_english %in% c("Chongqing","Sichuan","Guizhou","Yunnan","Tibet")~"Southwest",
                          province_english %in% c("Shaanxi","Gansu","Qinghai","Ningxia","Xinjiang")~"Northwest",
                          province_english %in% c("Heilongjiang","Jilin","Liaoning")~ "Northeast")) %>%
  st_as_sf( sf_column_name = 'geometry')



-----------------------------------------------------------------------------------
-------------------------------------------------------------------------------------



#Tiantian's data for year 2014
Li14 <- read.csv("2014_pm.csv")
pm_in <- Li14

# merge the two 
pm_in <- cbind( grid[, .( X, Y)], pm_in) 

# create sf object
# grid cells are not regular, so I can't create a raster
# this creates a dataset of points
pm25.sf <- st_as_sf( pm_in, coords = c( 'X', 'Y'), crs = 'WGS84')

#spatial plot
plot(pm25.sf)

-------------------------------------------------------------------
 

#save datasets
fwrite(pm_in, file = "PM_2014_Li.csv", row.names = F)








-----------------------------------------------------------------------------
----------------------------------------------------------------------------

  
  

#Repeat the same with Randall Martin Data
RM2014.r <- brick("RM2014.nc")
pm_in <- RM2014.r

# merge the two 
#pm_in <- cbind( grid, pm_in) 

# create sf object
pm25.poly <- rasterToPolygons(pm_in, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
pm25.sf <- st_as_sf( pm25.poly, coords = c( 'X', 'Y'), crs = 'WGS84')

#or create sf object#
pm25.sf <- st_as_sf( rasterToPolygons(pm_in), coords = c( 'X', 'Y'), crs = 'WGS84')

#spatial plot
plot(pm25.sf)

  --------------------------------------------------------------------

#read Randall-Martin data
RM2014.r <- brick("RM2014.nc")

#raster to points (spatial data frame)
rm14_pts <- rasterToPoints(RM2014.r, spatial = TRUE)

#convert to a conventional  dataframe
rm14_df  <- data.frame(rm14_pts)

#save dataset
fwrite(rm14_df, file = "PM_2014_RM.csv", row.names = F)


# merge with grid 
rm_grid <- cbind( grid[, .( X, Y)], rm14_df) 

#save dataset
fwrite(rm_grid, file = "PM_2014_RM_2.csv", row.names = F)




--------------------------------------------------------
-----------------------------------------------------------


  
  
  
###Huang Data###
#loading necessary libraries
library( sf)
library(sp)
library( raster)
library( data.table)
library( ggplot2)
library( viridis)
library(tiff)

#set directory
setwd("C:/Users/15712/Desktop/China_obs/Huang/PM25AnnualMean2008-2019")

#load raster into R
huang_14 <- raster("PM25_2014.tif") 

#view raster structure
huang_14

## convert to a df; first, to a SpatialPointsDataFrame
huang14_pts <- rasterToPoints(huang_14, spatial = TRUE)


#plot the dataframe
#ggplot() +
  #geom_raster(data = huang14_df , aes(x = x, y = y)) + 
  #ggtitle("Huang's Group's Annual Mean PM2.5 Data, Year:2014") +
  #coords = c( 'X', 'Y'), crs = 'WGS84'


#next, convert to shapefile using Chinese grid
pm25.sf <- st_as_sf( huang14_pts, coords = c( 'X', 'Y'), crs = 'WGS84')

#plot
plot(pm25.sf)

------------------------------------------------------------

#convert to a conventional  dataframe
huang_df  <- data.frame(huang14_pts)

# merge with grid 
huang_grid <- cbind( grid[, .( X, Y)], huang_df) 

#save datasets
fwrite(huang_grid, file = "PM_2014_Huang_2.csv")











