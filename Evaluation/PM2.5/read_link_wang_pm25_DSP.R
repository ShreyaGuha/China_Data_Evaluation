rm( list = ls())
library(sp)
library( raster)
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

# just to check—but this takes a while
# plot( china.shp)

# add an ID column
# china.shp$ID <- 1:nrow( china.shp)


## =============================================================
# define function to link locations in each year with the appropriate PM field
## =============================================================
# write a function that accepts year, file locations, and link locations, then outputs
#  PM exposure for each link location in a given year
link.fn <- 
  function( year_to_link,
            data.dir,
            locations.sf){
    
    # print the year
    print( year_to_link)
    
    ## ================================
    # read in the PM data
    ## ================================
    # define pattern to search for that matches the year
    pattern_y <- paste0( 'PM25_', year_to_link, '.tif')
    
    # list the files available
    wang.files <- 
      list.files( data.dir_wang, pattern = pattern_y, full.names = TRUE)
    
    # select correct files
    message( paste( 'Reading file', wang.files))
    
    # read in the file as raster
    wang_y <- raster( wang.files) 
    
    ## ================================
    #  transform locations spatially
    ## ================================

    # convert link location crs to match raster
    locations.sf.trns <- 
      st_transform( locations.sf,
                    crs = crs( wang_y))
    
    # do the extraction
    message( 'Performing the extraction')
    wang_y_loc <- 
      extract( wang_y, 
               locations.sf.trns,
               fun = mean, 
               weights = TRUE,
               exact = FALSE)
    
    ## ================================
    #  format the data for output
    ## ================================
    # create a data table of the results
    wang_i_link.dt <- 
      data.table( as.data.table( locations.sf.trns)[, .( NAME, CNTY_CODE, PYNAME, POST_COD)],
                  year = year_to_link,
                  PM_Wang = wang_y_loc[,1])
    
    # return data
    return( wang_i_link.dt)
  }

## =============================================================
# run the function for all years
## =============================================================
# define the data directory
# NOTE : change to match your system
# data.dir_wang <- "~/Library/CloudStorage/OneDrive-SharedLibraries-GeorgeMasonUniversity-O365Production/HAQ\ Lab-GRP\ -\ Documents/People/GuhaShreya/AQ_fields_china/Huang/PM25AnnualMean2008-2019"
data.dir_wang <- "D:/Professional/Projects/China_obs/Huang/PM25AnnualMean2008-2019"


## run the function for all years using lapply
location_pm_links.list <- 
  pbmcapply::pbmclapply(
    2008:2019,
    link.fn,
    data.dir = data.dir_wang,
    locations.sf = china.shp
  )

# combine to single data.table
location_pm_links.dt <- 
  rbindlist( location_pm_links.list)

fwrite( location_pm_links.dt,
           'D:/Professional/Projects/China_obs/Huang/DSP_avgPM_with_info.csv')

setwd("D:/Professional/Projects/China_obs/Huang")
location_pm_links.dt <- fread("D:/Professional/Projects/China_obs/Huang/DSP_avgPM_with_info.csv")

## =============================================================
#  investigate data: 
## =============================================================
library( ggplot2)

#adding geometry
location_pm_links.dt_geom <- cbind( location_pm_links.dt, china.shp$geometry)

#rename
library(dplyr)
location_pm_links.dt_geom <- location_pm_links.dt_geom %>%
  rename( PM_Huang = PM_Wang)
location_pm_links.dt_geom <- location_pm_links.dt_geom %>%
  rename( geometry = V2)
location_pm_links.dt <- location_pm_links.dt %>%
  rename( PM_Huang = PM_Wang)


#turn into spatial object
location_pm_links.sf <- 
  st_as_sf( location_pm_links.dt_geom)

# example plot exposure histogram by year
ggplot( location_pm_links.dt,
        aes( x = PM_Huang)) + 
  geom_histogram() + 
  facet_wrap( . ~ year) + 
  theme_minimal()


#plotting all years
library(ggplot2)
library(viridis)
ggplot( location_pm_links.sf,
        aes( color = PM_Huang, fill = PM_Huang)) + 
  geom_sf() + 
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  facet_wrap( . ~ year) +
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         axis.text = element_text(size = 5.5),
         legend.position="bottom", legend.box = "horizontal")




