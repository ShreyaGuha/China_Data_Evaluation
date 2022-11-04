rm( list = ls())
library( raster)
library(sp)
library( sf)
library( data.table)


## =============================================================
# create data set of locations and years
## =============================================================
# read in shape file of DSP locations
# be sure 7 files are contained in directory
    # quxian.dbf
    # quxian.prj
    # quxian.sbn
    # quxian.sbx
    # quxian.shp
    # quxian.shp.xml
    # quxian.shx
file_china.shp <- "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/2012??????shp"
china.shp <- st_read( file_china.shp, crs = 'WGS84') 

# define function to link locations in each year with the appropriate OZONE field
# write a function that accepts year, file locations, and link locations, then outputs
#  OZONE exposure for each link location in a given year
link.fn <- 
  function( year_to_link,
            data.dir,
            locations.sf){
    
    # print the year
    print( year_to_link)
    
    # define pattern to search for that matches the year
    pattern <- paste0( year_to_link, '_O3_v2.csv')
    
    #define directory
    TAP.dir <- "D:/Professional/Projects/China_obs/Ozone_TAP"
    
    #list files
    TAP.files <- list.files( TAP.dir, full.names = TRUE,
                             pattern = '_O3_v2.csv')
    
    # select correct files
    file_name <- grep( pattern, TAP.files, value = TRUE)
    
    # read file
    tap.i <- fread(file_name)
    
    # read in the file as raster, eliminate grid column
    tap_r <- rasterFromXYZ(tap.i[,.(X_Lon, Y_Lat,M8H_O3)])
    
    #add crs
    crs(tap_r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    #plot raster
    plot(tap_r)
    
    # convert link location crs to match tap data
    locations.sf.trns <- 
      st_transform( locations.sf,
                    crs = crs(tap_r))
    
    
    
    # do the extraction
    tap_r_loc <- 
      extract( tap_r, 
               locations.sf.trns,
               fun = mean,
               na.rm= TRUE,
               weights = TRUE,
               exact = FALSE)
    
    
    # create a data table of the results
    tap_i_link.dt <- 
      data.table( as.data.table( locations.sf.trns)[, .( NAME, CNTY_CODE, PYNAME, POST_COD)],
                  year = year_to_link,
                  o3_Tap = tap_r_loc[,1])
    
    # return data
    return( tap_i_link.dt)
  }


# define the data directory
data.dir <- "D:/Professional/Projects/China_obs/Ozone_TAP"


## run the function for all years using lapply
location_pm_links.list <- 
  pbmcapply::pbmclapply(
    2013:2019,
    link.fn,
    data.dir = TAP.dir,
    locations.sf = china.shp
  )

# combine to single data.table
location_pm_links.dt <- 
  rbindlist( location_pm_links.list)

fwrite( location_pm_links.dt,
        "D:/Professional/Projects/China_obs/Ozone_DSP.csv" )


#  investigate data: 
library( ggplot2)

# example plot exposure histogram by year
ggplot( location_pm_links.dt,
        aes( x = o3_Tap)) + 
  geom_histogram() + 
  facet_wrap( . ~ year) + 
  theme_minimal()


#Second time, with geometry included

file_china.shp <- "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/2012??????shp/quxian.shp"
china.shp <- st_read( file_china.shp, crs = 'WGS84') 

link.fn <- 
  function( year_to_link,
            data.dir,
            locations.sf){
    
    # print the year
    print( year_to_link)
    
    # define pattern to search for that matches the year
    pattern <- paste0( year_to_link, '_O3_v2.csv')
    
    #define directory
    TAP.dir <- "D:/Professional/Projects/China_obs/Ozone_TAP"
    
    #list files
    TAP.files <- list.files( TAP.dir, full.names = TRUE,
                             pattern = '_O3_v2.csv')
    
    # select correct files
    file_name <- grep( pattern, TAP.files, value = TRUE)
    
    # read file
    tap.i <- fread(file_name)
    
    # read in the file as raster
    tap_r <- rasterFromXYZ(tap.i[,.(X_Lon, Y_Lat,M8H_O3)])
    
    #add crs
    crs(tap_r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    #plot raster
    plot(tap_r)
    
    # convert link location crs
    locations.sf.trns <- 
      st_transform( locations.sf,
                    crs = crs(tap_r))
    
    
   # do the extraction
    tap_r_loc <- 
      extract( tap_r, 
               locations.sf.trns,
               fun = mean,
               na.rm= TRUE,
               weights = TRUE,
               exact = FALSE)
    
    
    #  format the data for output
    # create a data table of the results, geometry included
    tap_i_link.dt_2 <- 
      data.table( as.data.table( locations.sf.trns)[, .( NAME, CNTY_CODE, PYNAME, POST_COD, geometry)],
                  year = year_to_link,
                  o3_Tap = tap_r_loc[,1])
    
    # return data
    return( tap_i_link.dt_2)
  }

#set directory
data.dir <- "D:/Professional/Projects/China_obs/Ozone_TAP"


## run the function for all years using lapply
location_pm_links.list.2 <- 
  pbmcapply::pbmclapply(
    2013:2019,
    link.fn,
    data.dir = TAP.dir,
    locations.sf = china.shp
  )

# combine to single data.table
location_pm_links.dt.2 <- 
  rbindlist( location_pm_links.list.2)

#turn into spatial object
location_pm_links.sf <- 
  st_as_sf( location_pm_links.dt.2)

#plotting all years
library(ggplot2)
ggplot( location_pm_links.sf,
        aes( color = o3_Tap, fill = o3_Tap)) + 
  geom_sf() + 
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  facet_wrap( . ~ year) +
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         axis.text = element_text(size = 5.5),
         legend.position="bottom", legend.box = "horizontal")

