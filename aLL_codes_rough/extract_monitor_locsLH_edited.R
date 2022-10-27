library( raster)
library(sp)
library( sf)
library( data.table)
library( ggplot2)


## ================================================= ##
# observational data
## ================================================= ##

# read the observational data
setwd("C:/Users/15712/Desktop/China_obs/All_Data_PM2.5")
obs_dat <- fread("Observation_from_Hourly_14_22..csv")
#obs_dat <- fread( '~//Library/CloudStorage/OneDrive-SharedLibraries-GeorgeMasonUniversity-O365Production/HAQ\ Lab-GRP\ -\ Documents/People/GuhaShreya/AQ_fields_china/All_Data_PM2.5/Observation_from_Hourly_14_22.csv')

# shreya to check:lat & lon are characters but should be numeric
obs_dat$latitude <- as.numeric( obs_dat$latitude)
obs_dat$longitude <- as.numeric( obs_dat$longitude)

# shreya to check-missing values in lat/lon
summary( obs_dat)

# convert to spatial object, only select records with non-NA lat/lon
obs_dat.use <- obs_dat[!is.na( latitude) & 
                         !is.na( longitude)]

# shreya to check whether wgs84 is appropriate
#wgs84 is apt
obs_dat.sf <- 
  st_as_sf( obs_dat.use, 
            coords = c( 'longitude', 'latitude'), crs = 'WGS84')

# the raster extractions work best with SpatialPointsDataFrame
obs_dat.sp <- as_Spatial( obs_dat.sf)

# check out a spatial plot
# shreya to check - what about the other years?
ggplot( obs_dat.sf,
        aes( color = PM)) + 
  geom_sf() + 
  facet_wrap( . ~ year)

# check out some box plots
ggplot( obs_dat.sf,
        aes( x = year, 
             y = PM, group = year)) + 
  geom_boxplot()

## ================================================= ##
# read prediction rasters and merge with obs
## ================================================= ##

## where is Huang's data
#huang.dir <- "~/Library/CloudStorage/OneDrive-SharedLibraries-GeorgeMasonUniversity-O365Production/HAQ\ Lab-GRP\ -\ Documents/People/GuhaShreya/AQ_fields_china/Huang/PM25AnnualMean2008-2019"

huang.dir <- "C:/Users/15712/Desktop/China_obs/Huang/PM25AnnualMean2008-2019"
huang.files <- list.files( huang.dir, full.names = TRUE)

## to - do: wrap in for loop to extract data in all years
#Loop over file list importing them and binding them together
for (i in 1:length(huang.files)) 
{
  file_name <- huang.files[i]
  file_raster <- raster(huang.files[i]) 
  #can also use "stack" function to bind data, but this works fine
  #loop the entire function here, ignore later i's
  
}


# read in single raster
#load raster into R
huang_i <- raster(huang.files[i]) 

# select observations in 2014
obs_dat.sp20i <- 
  obs_dat.sp[ obs_dat.sp$year == 20i,]

# convert observation crs to match raster
obs_dat.sp20i.trns <- 
  spTransform( obs_dat.sp20i,
               CRSobj = crs( huang_14))

# check that crs matches
crs( huang_i, asText = TRUE) == crs( obs_dat.sp20i.trns, asText = TRUE)

# do the extraction
huang_i_obs <- 
  extract( huang_i, obs_dat.sp20i.trns)

# create a data table of the results
huang_i_obs.dt <- 
  data.table( station.name = obs_dat.sp20i.trns$station.name,
              year = 20i,
              PM = obs_dat.sp20i.trns$PM,
              PM_huang = huang_i_obs)



# plot the results
plot( huang_14_obs.dt$PM, huang_14_obs.dt$PM_huang)


## ================================================= ##
# repeat for RM data
## ================================================= ##
## where is Huang's data
RM.dir <- "C:/Users/15712/Desktop/China_obs/RM"
RM.files <- list.files( RM.dir, full.names = TRUE)

## to - do: wrap in for loop to extract data in all years
for (i in 1:length(RM.files)) 
{
  file_name <- RM.files[i]
  file_raster <- raster(RM.files[i]) 
}
#can't loop function, ignore i's
# read in single raster
#load raster into R
RM_i <- raster(RM.files[i]) 

# select observations in 2014
# note:done with correct year, we have the data now
obs_dat.sp20i <- 
  obs_dat.sp[ obs_dat.sp$year == 200i,]

# convert observation crs to match raster
obs_dat.sp2014.trns <- 
  spTransform( obs_dat.sp20i,
               CRSobj = crs( RM_i))

# check that crs matches
crs( RM_i, asText = TRUE) == crs( obs_dat.sp20i.trns, asText = TRUE)

# do the extraction
RM_i_obs <- 
  extract( RM_i, obs_dat.sp20i.trns)

# create a data table of the results
RM_i.dt <- 
  data.table( station.name = obs_dat.sp20i.trns$station.name,
              year = 200i,
              PM = obs_dat.sp20i.trns$PM,
              PM_RM = RM_i_obs)



# plot the results
plot( RM_03.dt$PM, RM_03.dt$PM_RM)


## ================================================= ##
# Then, merge the datasets
## ================================================= ##
obs_rasterfields_all <- 
  merge( huang_14_obs.dt, RM_03.dt,
         by = c( 'station.name', 'year', 'PM'),
         all = TRUE) %>%
  merge( obs_dat.use[, .( station.name, year, longitude, latitude)],
         by = c( 'station.name', 'year'))

summary( obs_rasterfields_all)
summary( obs_rasterfields_all[is.na( PM_huang)])
summary( obs_rasterfields_all[is.na( PM_RM)])

# plot comparisons by year
ggplot( obs_rasterfields_all)













