#set working directory
setwd("C:/Users/15712/Desktop/China_obs/2015")


#########------Observational Data----------#######


#read observational data
obs_dat <- fread("PM_2015_annual_mean_station_id.csv")

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


#############--------------TAP Data------------#############

# read the tap data
tap <- fread("tap_2015.csv")

#rename lat/lon
tap <- tap %>% rename(latitude = Latitude)
tap <- tap %>% rename(longitude = Longitude)


#set memory limit, this is very crucial
memory.limit(size = 1200000000)

#only select records with non-NA lat/lon
#not sure if this step is necessary, as summary(tap) is not running
tap.use <- tap[!is.na( latitude) & 
                         !is.na( longitude)]

#convert to spatial object
tap.sf <- 
  st_as_sf( tap.use, 
            coords = c( 'longitude', 'latitude'), crs = 'WGS84')


#the raster extractions work best with SpatialPointsDataFrame
tap.sp <- as_Spatial( tap.sf)

# check out a spatial plot
ggplot( tap.sf,
        aes( color = PM)) + 
  geom_sf() + 
  facet_wrap( . ~ year)

#check out some box plots
ggplot( tap.sf,
        aes( x = year, 
             y = PM, group = year)) + 
  geom_boxplot()

# select closest value from PM dataset to each observation
# this step is a little slow, but not too bad.
tap.use$obs_nearest <- 
  st_nearest_feature( obs_dat.sf,
                      tap.sf)

#merge all

obs_rasterfields_all <- 
  merge( obs_dat.use, tap.use,
         by = c( 'station.name', 'year'),
         all = TRUE)


## ================================================= ##
# Do the evaluation
## ================================================= ##
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


