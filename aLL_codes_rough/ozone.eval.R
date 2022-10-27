##Ozone data evaluation
rm(list = ls())

library(sp)
library(raster)
library(sf)
library(data.table)
library(dplyr)
library(tools)
library(ggplot2)
library(lubridate)

#read ozone data
in_data_daily_o3 <- fread("D:/Professional/Projects/China_obs/Haotian/Hao_o3.csv")

#check range of data to compare to other datasets
range(in_data_daily_o3$date)

#modify format of date column
in_data_daily_o3$date <- as.Date(in_data_daily_o3$date)

#remove unnecessary columns
daily_o3 <- subset(in_data_daily_o3, select = -c(variable, N))

#read station data
station <- read.csv("D:/Professional/Projects/China_obs/stations.csv")

#merge coordinates
obs_o3 <- merge(daily_o3,station, by = "sta.ID")

#rename columns
#rename
obs_o3 <- obs_o3 %>% rename(ozone = day_mean)

#save data
fwrite(obs_o3, file = "obs_o3.csv")


#####

#Data Comparison

#setting specific directory
setwd("D:/Professional/Projects/China_obs/Haotian")

#read saved data
obs_dat <- fread("D:/Professional/Projects/China_obs/Haotian/obs_o3.csv")

#find year
obs_dat$year <- year(obs_dat$date)

#subset to Tiantian's size
obs_dat_sub <- obs_dat[year <= 2017]

#check summary
summary( obs_dat_sub)
#lots of missing data as we are using old station data

#only select records with non-NA lat/lon
obs_dat.use <- obs_dat_sub[!is.na( latitude) & 
                         !is.na( longitude)]

#convert to spatial object, WGS 84 is apt
obs_dat.sf <- 
  st_as_sf( obs_dat.use, 
            coords = c( 'longitude', 'latitude'), crs = 'WGS84')

#the raster extractions work best with SpatialPointsDataFrame
obs_dat.sp <- as_Spatial( obs_dat.sf)

#extract years
obs_dat.sf$year <- year(obs_dat.sf$date)

# check out a spatial plot
ggplot( obs_dat.sf,
        aes( color = ozone)) + 
  geom_sf() + 
  facet_wrap( . ~ year)

# check out some box plots
ggplot( obs_dat.sf,
        aes( x = year, 
             y = ozone, group = year)) + 
  geom_boxplot()


###we can skip all this for now###
##Regional evaluation##

##Preparation for coordinate data

setwd("D:/Professional/Projects/China_obs/Ozone_Tiantian")

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




#Joining coordinate info with observation data for further evaluation

# join provinces with observations
obs_provinces.sf <- 
  st_join( china.shp.reg.sf,
           obs_dat.sf) 


#cannot code lines 158-179 due to system issues
#set memory limit before next step, that requires lots of time & memory
#PC crashed twice while doing that
memory.limit(size = 1200000000)


####skip####
#skipping lines 166-176 to avoid memory issues
#using st feature
obs_coordinates <- 
  st_coordinates( obs_provinces.sf) 
#still having memory issues with this step
#cannot allocate vector of size 4.3 Gb

#data table approach
obs_coordinates <- as.data.table(obs_coordinates)

#formatting the coordinate names
setnames( obs_coordinates, c( 'X', 'Y'), c( 'latitude', 'longitude'))




#formatting the data table
obs_provinces.dt <- 
  data.table( obs_provinces.sf)[, .( sta.ID, year, province_english, region, day_mean)] %>%
  na.omit()

#rename columns
obs_provinces.dt <- obs_provinces.dt %>% rename(station.name = sta.ID)

#save data
fwrite (obs_provinces.dt, file = "obs.ozone.csv")

####skip part ends####


###Prediction Data###
#read prediction rasters and merge with obs


##Tiantian Li's Data
## where is Tiantian's data
LI.dir <- "D:/Professional/Projects/China_obs/Ozone_Tiantian"
LI.files <- list.files( LI.dir, full.names = TRUE,
                        pattern = 'o.csv')

# there's a grid file we need too
LI.grid.f <- fread( "D:/Professional/Projects/China_obs/Ozone_Tiantian/grid.csv")

# establishish empty data.table to fill with for loop
LI_data_obs <- data.table()

for (i in 2013:2017) {
  print( i)
  
  # define pattern to serch for that matches the year
  pattern <- paste0( i, 'o.csv')
  
  # select correct files
  file_name <- grep( pattern, LI.files, value = TRUE)
  
  # read file
  Li.i <- fread( file_name)
  
  # merge with grid
  Li.i.dt <- data.table( LI.grid.f[, .( X, Y)], Li.i) 
  
  # grid cells are not regular, so I can't create a raster (rasterFromXYZ produces error)
  # so, instead we will create a dataset of points
  # and then select closest point to each observation
  
  # create sf object
  # this creates a dataset of points
  LI.i.sf <- st_as_sf( Li.i.dt, 
                       coords = c( 'X', 'Y'), 
                       crs = 'WGS84')
  
  # select observations in appropriate year
  # note:done with correct year, we have the data now
  obs_dat.sf20i <- 
    obs_dat.sf[ obs_dat.sf$year == i,]
  
  # convert observation crs to match raster
  obs_dat.sf20i.trns <- 
    st_transform( obs_dat.sf20i, crs( LI.i.sf))
  
  # select closest value from PM dataset to each observation
  # this step is a little slow, but not too bad.
  obs_nearest <- st_nearest_feature( obs_dat.sf20i.trns,
                        LI.i.sf)
  
  # create a data table of the results
  LI_obs.i.dt <- 
    data.table( station.name = obs_dat.sf20i.trns$sta.ID,
                year = i,
                ozone_LI = Li.i.dt[obs_nearest]$aa)
  
  # rbind with existing data table
  LI_data_obs <- 
    rbind( LI_data_obs,
           LI_obs.i.dt)
  
}

#save data
fwrite(LI_data_obs, file = "Li.csv")

#rename columns
obs_dat_sub <- obs_dat_sub %>% rename(station.name = sta.ID)

#save data
fwrite(obs_dat_sub, file = "obs.ozone.csv")




rm(list = ls())

#read data
obs_provinces.dt <- fread("obs.ozone.csv")
LI_data_obs <- fread("Li.csv")

#set memory limit to avoid error messages involving negative length vectors
memory.limit(size = 12000000000)
#cannot code the next steps as I don't have obs_provinces.dt
#merge all datasets
#ozone_eval <- merge(LI_data_obs, obs_provinces.dt, 
                    #by = c("station.name", "year"),
                    #allow.cartesian=TRUE)


#ozone_eval <- cbind(LI_data_obs, obs_provinces.dt)

#save data
fwrite(ozone_eval, file = "ozone_eval.csv")

#checking memory usage
library(pryr)
mem_used()

###
#Do the evaluation
#did not edit this part much as I could not join the data

ozone_eval <- fread("ozone_eval.csv")

# first, melt for easier manipulation
obs_rasterfields_all.m <- 
  melt( ozone_eval,
        id.vars = c( 'station.name', 'province_english', 'region', 'year', 'day_mean'),
        value.name = 'ozone',
        variable.name = 'ozone_LI')

# plot comparisons by year
ggplot( obs_rasterfields_all.m[year <= 2013],
        aes( x = PM,
             y = PM_modeled,
             color = model)) + 
  geom_abline( slope = 1, intercept = 0) + 
  geom_point( ) + 
  geom_smooth( se = FALSE, method = 'lm') + 
  scale_color_brewer( palette = 'Dark2') +
  facet_grid( model ~ year) + 
  theme_minimal()

# define an evaluation function
evals.fn <- function( Yhat, Yact){
  # create consitent data.table
  eval.dt <- data.table( Yhat, Yact) %>%
    na.omit()
  
  num.diff <- sum( eval.dt$Yhat - eval.dt$Yact)
  abs.diff <- sum( abs( eval.dt$Yhat - eval.dt$Yact))
  denom <- sum( eval.dt$Yact)
  N <- nrow( eval.dt)
  
  metrics <- data.table( N = as.double( ifelse( N == 0, NaN, N)),
                         NMB_pct = 100 * (num.diff / denom),
                         NME_pct = 100 * ( abs.diff / denom),
                         MB_µgm3 = num.diff / N,
                         ME_µgm3 = abs.diff / N,
                         RMSE_µgm3 = sqrt( sum( ( eval.dt$Yhat - eval.dt$Yact) ^ 2) / N),
                         R2 = cor( eval.dt$Yhat, eval.dt$Yact, method = 'pearson') ^ 2)
  return( metrics)
}

# do the evaluation by year
eval_out <- 
  obs_rasterfields_all.m[, evals.fn( ozone_modeled, ozone), by = .( model, year)]

# repeat the evaluation for only stations operating in 2013
eval_out.2013sta <- 
  obs_rasterfields_all.m[ station.name %in% sta_2013,
                          evals.fn( ozone_modeled, ozone), 
                          by = .( model, year)]

# to plot, melt into long form
eval_out.m <- 
  melt( eval_out,
        id.vars = c( 'model', 'year'))
eval_out.2013sta.m <- 
  melt( eval_out.2013sta,
        id.vars = c( 'model', 'year'))

# plot the evaluation
ggplot( eval_out.m[year <= 2013 & 
                     model %in% c( 'ozone_Li', 'PM_Wang')],
        aes( x = year,
             y = value,
             color = model,
             group = model)) + 
  geom_hline( yintercept = 0) +
  geom_line() + 
  scale_color_brewer( palette = 'Dark2') +
  facet_wrap( . ~ variable, ncol = 1,
              scales = 'free_y') + 
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'right')

# plot the evaluation
ggplot( eval_out.2013sta.m[year <= 2013 & 
                             model %in% c( 'ozone_Li', 'PM_Wang')],
        aes( x = year,
             y = value,
             color = model,
             group = model)) + 
  geom_hline( yintercept = 0) +
  geom_line() + 
  scale_color_brewer( palette = 'Dark2') +
  facet_wrap( . ~ variable, ncol = 1,
              scales = 'free_y') + 
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'right')


