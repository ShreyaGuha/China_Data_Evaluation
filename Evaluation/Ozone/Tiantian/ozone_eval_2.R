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
obs_o3 <- fread("D:/Professional/Projects/China_obs/Haotian/annual_obs_ozone.csv")

#subset to Tiantian's size
obs_dat_sub <- obs_o3[year <= 2017]

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
  Li.i <- fread(file_name)
  
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
                ozone_obs = obs_dat.sf20i.trns$ozone,
                ozone_LI = Li.i.dt[obs_nearest]$aa)
  
  # rbind with existing data table
  LI_data_obs <- 
    rbind( LI_data_obs,
           LI_obs.i.dt)
  
}


#save data
fwrite(LI_data_obs, file = "Li_and_obs.csv")


# first, melt for easier manipulation
obs_rasterfields_all.m <- 
  melt( LI_data_obs,
        id.vars = c( 'station.name', 'year', 'ozone_obs'),
        value.name = 'ozone_modeled',
        variable.name = 'model')


#save data
fwrite(obs_rasterfields_all.m, file = "ozone_li_eval.csv")

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
fwrite(eval_out, file = "li_eval.csv")


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







