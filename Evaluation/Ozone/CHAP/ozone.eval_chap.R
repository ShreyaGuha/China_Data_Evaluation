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
library(fst)

#Data Comparison

#setting specific directory
setwd("D:/Professional/Projects/China_obs/Haotian")

#read saved data
obs_o3 <- read.fst("D:/Professional/Projects/China_obs/Haotian/Haotian_ozone_mda8h_stat.fst")

#adjust
obs_o3$year <- as.integer(obs_o3$year)
obs_o3$mon.m <- as.numeric(obs_o3$mon.m)
obs_o3$day <- as.numeric(obs_o3$day)
obs_o3$date <- as.Date(obs_o3$date)

#convert to data table
obs_dat_sub <- as.data.table(obs_o3)

#only select records with non-NA lat/lon
obs_dat.use <- obs_dat_sub[!is.na( latitude) & 
                             !is.na( longitude)]

#convert to spatial object, WGS 84 is apt
obs_dat.sf <- 
  st_as_sf( obs_dat.use, 
            coords = c( 'longitude', 'latitude'), crs = 'WGS84')




##CHAP Data
## where is CHAP data
CHAP.dir <- "D:/Professional/Projects/China_obs/CHAP_ozone"
CHAP.files <- list.files( CHAP.dir, full.names = TRUE,
                         pattern = '_V1.nc')

# establishish empty data.table to fill with for loop
chap_data_obs <- data.table()

for (i in 2013:2020) {
  print( i)
  
  # define pattern to serch for that matches the year
  pattern <- paste0( i, '_V1.nc')
  
  # select correct files
  file_name <- grep( pattern, CHAP.files, value = TRUE)
  
  # read file
  chap.i <- raster(file_name)
  
  # select observations in appropriate year
  # note:done with correct year, we have the data now
  obs_dat.sf20i <- 
    obs_dat.sf[ obs_dat.sf$year == i,]
  
  # check that crs matches
  crs( chap.i, asText = TRUE) == crs( obs_dat.sf20i, asText = TRUE)
  
  # do the extraction
  chap_i_obs <- 
    extract( chap.i, obs_dat.sf20i)
  
  
  # create a data table of the results
  chap_obs.i.dt <- 
    data.table( station.name = obs_dat.sf20i$station,
                year = i,
                ozone_obs = obs_dat.sf20i$mda_o3,
                ozone_chap =  chap_i_obs)
  
  # rbind with existing data table
  chap_data_obs <- 
    rbind( chap_data_obs,
           chap_obs.i.dt)
  
}

#save data
fwrite(chap_data_obs, file = "CHAP_and_obs.csv")


# first, melt for easier manipulation
obs_rasterfields_all.m <- 
  melt( chap_data_obs,
        id.vars = c( 'station.name', 'year', 'ozone_obs'),
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
fwrite(eval_out, file = "chap_eval.csv")

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






