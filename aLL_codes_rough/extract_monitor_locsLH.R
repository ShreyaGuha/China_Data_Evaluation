library( raster)
library( sf)
library( data.table)
library( ggplot2)

# "regular expressions"
stat.nameX <- c( 'X123X', 'X000X', 'X222X')
gsub( '^X', '', stat.nameX)

substr( stat.nameX, 2, 4)

mean()

## ================================================= ##
# observational data
## ================================================= ##

# read the observational data
obs_dat <- fread( '~//Library/CloudStorage/OneDrive-SharedLibraries-GeorgeMasonUniversity-O365Production/HAQ\ Lab-GRP\ -\ Documents/People/GuhaShreya/AQ_fields_china/All_Data_PM2.5/Observation_from_Hourly_14_22.csv')

# shreya to check—lat & lon are characters but should be numeric
obs_dat$latitude <- as.numeric( obs_dat$latitude)
obs_dat$longitude <- as.numeric( obs_dat$longitude)

# shreya to check-missing values in lat/lon
summary( obs_dat)

# convert to spatial object, only select records with non-NA lat/lon
obs_dat.use <- obs_dat[!is.na( latitude) & 
                         !is.na( longitude)]

# shreya to check whether wgs84 is appropriate
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
huang.dir <- "~/Library/CloudStorage/OneDrive-SharedLibraries-GeorgeMasonUniversity-O365Production/HAQ\ Lab-GRP\ -\ Documents/People/GuhaShreya/AQ_fields_china/Huang/PM25AnnualMean2008-2019"
huang.files <- list.files( huang.dir, full.names = TRUE)

## to - do: wrap in for loop to extract data in all years

# read in single raster
#load raster into R
huang_14 <- raster(huang.files[1]) 

# select observations in 2014
obs_dat.sp2014 <- 
  obs_dat.sp[ obs_dat.sp$year == 2014,]

# convert observation crs to match raster
obs_dat.sp2014.trns <- 
  spTransform( obs_dat.sp2014,
               CRSobj = crs( huang_14))

# check that crs matches
crs( huang_14, asText = TRUE) == crs( obs_dat.sp2014.trns, asText = TRUE)

# do the extraction
huang_14_obs <- 
  extract( huang_14, obs_dat.sp2014.trns)

# create a data table of the results
huang_14_obs.dt <- 
  data.table( station.name = obs_dat.sp2014.trns$station.name,
              year = 2014,
              PM = obs_dat.sp2014.trns$PM,
              PM_huang = huang_14_obs)

# plot the results
plot( huang_14_obs.dt$PM, huang_14_obs.dt$PM_huang)


## ================================================= ##
# repeat for RM data
## ================================================= ##
## where is Huang's data
RM.dir <- "~/Library/CloudStorage/OneDrive-SharedLibraries-GeorgeMasonUniversity-O365Production/HAQ\ Lab-GRP\ -\ Documents/People/GuhaShreya/AQ_fields_china/RM"
RM.files <- list.files( RM.dir, full.names = TRUE,
                        pattern = 'V4CH03')

## to - do: wrap in for loop to extract data in all years

# read in single raster
#load raster into R
RM_03 <- raster(RM.files[4]) 

# select observations in 2014
# note—obviously this is the incorrect year, need to change
obs_dat.sp2014 <- 
  obs_dat.sp[ obs_dat.sp$year == 2014,]

# convert observation crs to match raster
obs_dat.sp2014.trns <- 
  spTransform( obs_dat.sp2014,
               CRSobj = crs( RM_03))

# check that crs matches
crs( RM_03, asText = TRUE) == crs( obs_dat.sp2014.trns, asText = TRUE)

# do the extraction
RM_03_obs <- 
  extract( RM_03, obs_dat.sp2014.trns)

# create a data table of the results
RM_03.dt <- 
  data.table( station.name = obs_dat.sp2014.trns$station.name,
              year = 2014,
              PM = obs_dat.sp2014.trns$PM,
              PM_RM = RM_03_obs)

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













