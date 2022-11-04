##Ozone data evaluation
rm(list = ls())

#load necessary libraries
library(sp)
library(raster)
library(sf)
library(data.table)
library(dplyr)
library(tools)
library(ggplot2)
library(lubridate)

#Data Comparison

#setting specific directory
setwd("D:/Professional/Projects/China_obs/Haotian")

#read saved data
obs_dat <- read.fst("D:/Professional/Projects/China_obs/Haotian/Haotian_ozone_mda8h.fst")

#find year
#obs_dat$year <- year(obs_dat$date)

#create yearwise summary
#year.summary <- aggregate(obs_dat$ozone, list(obs_dat$year, obs_dat$sta.ID), mean)

#renaming columns
#names(year.summary)[1] <- 'year'
#names(year.summary)[2] <- 'sta.ID'
#names(year.summary)[3] <- 'ozone'

#read station data
station <- read.csv("D:/Professional/Projects/China_obs/stations.csv")
names(station)[1] <- 'station'

#merge coordinates
obs_o3 <- merge(obs_dat,station, by = "station")

#save data
write.fst(obs_o3, "D:/Professional/Projects/China_obs/Haotian/Haotian_ozone_mda8h_stat.fst")


#Afresh

#read saved data
obs_o3 <- read.fst("D:/Professional/Projects/China_obs/Haotian/Haotian_ozone_mda8h_stat.fst")

#adjust
obs_o3$year <- as.integer(obs_o3$year)
obs_o3$mon.m <- as.numeric(obs_o3$mon.m)
obs_o3$day <- as.numeric(obs_o3$day)
obs_o3$date <- as.Date(obs_o3$date)

#subset to TAP's size
obs_dat_sub <- subset(obs_o3, year <= 2019)

#check summary
summary( obs_dat_sub)
#lots of missing data as we are using old station data

#convert to data table
obs_dat_sub <- as.data.table(obs_dat_sub)

#only select records with non-NA lat/lon
obs_dat.use <- obs_dat_sub[!is.na( latitude) & 
                             !is.na( longitude)]

#convert to spatial object, WGS 84 is apt
obs_dat.sf <- 
  st_as_sf( obs_dat.use, 
            coords = c( 'longitude', 'latitude'), crs = 'WGS84')




##TAP Data
## where is TAP data
TAP.dir <- "D:/Professional/Projects/China_obs/Ozone_TAP"
TAP.files <- list.files( TAP.dir, full.names = TRUE,
                        pattern = '_O3_v2.csv')

# establishish empty data.table to fill with for loop
tap_data_obs <- data.table()

for (i in 2013:2019) {
  print( i)
  
  # define pattern to serch for that matches the year
  pattern <- paste0( i, '_O3_v2.csv')
  
  # select correct files
  file_name <- grep( pattern, TAP.files, value = TRUE)
  
  # read file
  tap.i <- fread(file_name)
  
  #data table
  tap.i.dt <- data.table(tap.i) 
  
  # create sf object
  # this creates a dataset of points
  tap.i.sf <- st_as_sf( tap.i.dt, 
                       coords = c( 'X_Lon', 'Y_Lat'), 
                       crs = 'WGS84')
  
  # select observations in appropriate year
  # note:done with correct year, we have the data now
  obs_dat.sf20i <- 
    obs_dat.sf[ obs_dat.sf$year == i,]
  
  # convert observation crs to match raster
  obs_dat.sf20i.trns <- 
    st_transform( obs_dat.sf20i, crs( tap.i.sf))
  
  # select closest value from PM dataset to each observation
  # this step is a little slow, but not too bad.
  obs_nearest <- st_nearest_feature( obs_dat.sf20i.trns,
                                     tap.i.sf)
  
  # create a data table of the results
  tap_obs.i.dt <- 
    data.table( station.name = obs_dat.sf20i.trns$sta.ID,
                year = i,
                ozone_obs = obs_dat.sf20i.trns$mda_o3,
                ozone_tap = tap.i.dt[obs_nearest]$M8H_O3)
  
  # rbind with existing data table
  tap_data_obs <- 
    rbind( tap_data_obs,
           tap_obs.i.dt)
  
}

#save data
fwrite(tap_data_obs, file = "TAP_and_obs.csv")

tap_data_obs <- fread("D:/Professional/Projects/China_obs/Haotian/TAP_and_obs.csv")

# first, melt for easier manipulation
obs_rasterfields_all.m <- 
  melt( tap_data_obs,
        id.vars = c( 'year', 'ozone_obs'),
        value.name = 'ozone_modeled',
        variable.name = 'model')


# plot comparisons by year
ggplot( obs_rasterfields_all.m,
        aes( x = ozone_obs,
             y = ozone_modeled,
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
  obs_rasterfields_all.m[, evals.fn( ozone_modeled, ozone_obs), by = .( model, year)]

#save data
fwrite(eval_out, file = "tap_eval.csv")

# to plot, melt into long form
eval_out.m <- 
  melt( eval_out,
        id.vars = c( 'model', 'year'))


#plot the evaluation
ggplot( eval_out.m,
        aes( x = year,
             y = value,
             color = model,
             group = variable)) +       
  geom_hline( yintercept = 0) +
  geom_line() + 
  scale_color_brewer( palette = 'Dark2') +
  facet_wrap( . ~ variable, ncol = 1,
              scales = 'free_y') +
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'right')












