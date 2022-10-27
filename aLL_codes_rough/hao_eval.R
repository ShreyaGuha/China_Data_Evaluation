library( sf)
library(sp)
library( raster)
library( data.table)
library( ggplot2)
library( viridis)
library( pbmcapply)
library( dplyr)
library(ncdf4)
library(lubridate)

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


####
#Haotian data
#set directory
setwd("C:/Users/15712/Desktop/China_obs/All_Data_PM2.5")

#read data
pm_hao <- read.csv("Hao_pm.csv")

#subset to remove year 2013
pm_hao$date <- as.Date(pm_hao$date)
pm_hao$year <- year(pm_hao$date)
pm_in <- subset(pm_hao, date(year)!= 2013)
#Error

# merge the two 
pm_in <- cbind( grid[, .( X, Y)], pm_in) 

#rename lat/lon
pm_in <- pm_in %>% rename(latitude = Y)
pm_in <- pm_in %>% rename(longitude = X)


# create sf object
# grid cells are not regular, so I can't create a raster
# this creates a dataset of points
pm_in.sf <- st_as_sf( pm_in, coords = c( 'longitude', 'latitude'), crs = 'WGS84')




####
#read observational data
obs_dat <- fread("Observation_from_Hourly_14_22..csv")

#coordinates should be numeric, not characters
obs_dat$latitude <- as.numeric( obs_dat$latitude)
obs_dat$longitude <- as.numeric( obs_dat$longitude)

#only select records with non-NA lat/lon
obs_dat.use <- obs_dat[!is.na( latitude) & 
                          !is.na( longitude)]

#convert to spatial object
#wgs84 is apt in this case
obs_dat.sf <- 
  st_as_sf( obs_dat.use, 
            coords = c( 'longitude', 'latitude'), crs = 'WGS84')

# the raster extractions work best with SpatialPointsDataFrame
obs_dat.sp <- as_Spatial( obs_dat.sf)

# check out a spatial plot
ggplot( obs_dat.sf,
        aes( color = PM)) + 
  geom_sf() + 
  facet_wrap( . ~ year)

# check out some box plots
ggplot( obs_dat.sf,
        aes( x = year, 
             y = PM, group = year)) + 
  geom_boxplot()



####
##Closest data
# select closest value from PM dataset to each observation
# this step is a little slow, but not too bad.
pm_in$obs_nearest <- 
  st_nearest_feature( obs_dat.sf,
                      pm_in.sf )

#merge all
obs_rasterfields_all <- 
  merge( obs_dat.use, pm_in,
         by = c( 'station.name', 'year'),
         all = TRUE)


#####
##Evaluation

# first, melt for easier manipulation
obs_rasterfields_all.m <- 
  melt( obs_rasterfields_all,
        id.vars = c( 'station.name', 'latitude', 'longitude', 'year', 'PM'),
        value.name = 'PM_modeled',
        variable.name = 'model')

# plot comparisons by year
ggplot( obs_rasterfields_all.m,
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
  obs_rasterfields_all.m[, evals.fn( PM_modeled, PM), by = .( model, year)]

# to plot, melt into long form
eval_out.m <- 
  melt( eval_out,
        id.vars = c( 'model', 'year'))

# plot the evaluation
ggplot( eval_out.m,
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
         legend.position = c( .9, .9))



