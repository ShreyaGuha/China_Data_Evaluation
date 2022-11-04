library( sf)
library( raster)
library( data.table)
library( ggplot2)
library( viridis)
library( pbmcapply)

# try for tiantian's data -- download
file1 <- 'https://zenodo.org/record/4009308/files/2005_pm.rar?download=1'
destfile.all <- '~/Dropbox/GeorgeMason/Grants/2020/HEI_Accountability/Data/pm_data'
# download.file( file1, destfile = file.path(destfile.all, '2005_pm.rar'))

# read the grid files
grid <- fread( file.path(destfile.all, 'grid.csv'))

# get chinese provinces spatial info
# downloaded from here: https://uploads.cosx.org/2009/07/chinaprovinceborderdata_tar_gz.zip
# link found here: https://liuyanguu.github.io/post/2020/06/12/ggplot-us-state-and-china-province-heatmap/#china-map-by-province
# I'm guessing the crs is WGS84-should confirm
file_china.shp <- '~/Dropbox/GeorgeMason/Grants/2020/HEI_Accountability/Data/pm_data/china-province-border-data/bou2_4p.shp'
china.shp <- st_read( file_china.shp, crs = 'WGS84') 
plot( china.shp[,'NAME'])


##============================================================================##
#Anthony's code to interpret the province name and categorize
##============================================================================##
china.shp = data.table::setDT(china.shp)
data.table::setnames(china.shp, "NAME", "province")
# transform to UTF-8 coding format
china.shp[, province:=iconv(province, from = "GBK", to = "UTF-8")] 
# add the province EN, CH label file
china.shp[, province:= as.factor(province)]
province_CH<-china.shp[, levels(province)] # the CH are in UTF-8 code
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
china.shp<-left_join(china.shp,province_name,by="province")

# categorize regions in China(common practice)
china.shp<-china.shp %>% 
  mutate(region=case_when(province_english %in% c("Shanghai","Jiangsu","Zhejiang","Anhui",
                                                  "Shandong","Fujian","Jiangxi","Taiwan") ~ "East",
                          province_english %in% c("Beijing","Tianjin","Hebei","Shanxi","Inner Mongolia") ~ "North",
                          province_english %in% c("Henan","Hubei","Hunan") ~ "Central",
                          province_english %in% c("Guangdong","Guangxi","Hainan","Hong Kong") ~ "South",
                          province_english %in% c("Chongqing","Sichuan","Guizhou","Yunnan","Tibet")~"Southwest",
                          province_english %in% c("Shaanxi","Gansu","Qinghai","Ningxia","Xinjiang")~"Northwest",
                          province_english %in% c("Heilongjiang","Jilin","Liaoning")~ "Northeast"))


## ============================================== ##
# write a function to read the data and spatially average to the provinces
## ============================================== ##

province_avg_pm.fn <- 
  function( y, file_location){
    message( paste('Reading data and spatially aggregating for', y))
    # read the data
    filename.in <- file.path( file_location,
                              paste0( y, '_pm.csv'))
    pm_in <- fread( filename.in)
    
    # merge the two 
    pm_in <- cbind( grid[, .( X, Y)], pm_in)
    
    # create sf object
    # grid cells are not regular, so I can't create a raster
    # this creates a dataset of points
    pm25.sf <- st_as_sf( pm_in, coords = c( 'X', 'Y'), crs = 'WGS84')
    
    # warning—this takes a while, so I'm only plotting a sample
    plot( pm25.sf[sample( 1:nrow( pm25.sf), 1000),])
    
    # spatially join by province
    # -- this is slow
    # break it up into 100 chunks
    chunks <- split( 1:nrow( pm25.sf), ceiling( 1:nrow( pm25.sf)/ 10000))
    
    message( 'Beginning spatial join')  
    pm25_provinces <- 
      pbmclapply( chunks,
                  function( c){
                    out <- 
                      st_join( china.shp,
                               pm25.sf[c,], left = FALSE) %>%
                      as.data.table()
                    # print( out)
                    return( out[,.( NAME, pm)])
                  }) %>% rbindlist()
    message( 'Spatial join complete')  
    
    # average by province
    pm25_avgs <- pm25_provinces[, .( pm = mean( pm, na.rm = TRUE),
                                     pm.min = min( pm, na.rm = TRUE),
                                     pm.max = max( pm, na.rm = TRUE),
                                     pm.25 = quantile( pm, .25, na.rm = TRUE),
                                     pm.75 = quantile( pm, .75, na.rm = TRUE)),
                                by = NAME]
    
    # add back the year
    pm25_avgs[, year := y]
    
    return( pm25_avgs)
  }


## ============================================== ##
# run the function
## ============================================== ##
# I only downloaded year 2005 and 2006 data
years <- 2005:2006

# use the function with lapply and rbindlist to apply to all years
# this function takes some time to run
province_avg_pm <- 
  lapply( years,
          province_avg_pm.fn,
          file_location = destfile.all) %>%
  rbindlist()

# merge back with spatial data and plot
province_avg_pm.sf <- 
  merge( province_avg_pm, china.shp,
         by = 'NAME', allow.cartesian = TRUE)

# make a spatial plot
province_avg_spatial.gg <- 
  ggplot( province_avg_pm.sf,
          aes( fill = pm, geometry = geometry)) + 
  geom_sf( color = NA) + 
  scale_fill_viridis( direction = -1) +
  expand_limits( fill = 0) +
  facet_wrap( . ~ year) + 
  theme_minimal() + 
  theme( axis.text = element_blank(),
         panel.grid = element_blank())
ggsave( '~/Dropbox/GeorgeMason/Grants/2020/HEI_Accountability/Data/pm_data/province_avg_spatial.png',
        province_avg_spatial.gg,
        height = 5, width = 12, unit = 'in', scale = .8)

# make a temporal plot
# one issue—the names look like they are in pinion(?), 
# so we'll need some help linking those to the English names
province_avg_ts.gg <- 
  ggplot( na.omit( province_avg_pm.sf),
        aes( x = as.factor( year), y = pm, 
             ymax = pm.75, ymin = pm.25,
             group = NAME)) + 
  geom_line( ) + 
  labs( y = expression( PM["2.5"]~', µg'~m^{"-3"})) +
  geom_errorbar( width = 0) +
  expand_limits( y = 0) +
  facet_wrap( . ~ NAME) + 
  theme_minimal() + 
  theme( axis.title.x = element_blank())

ggsave( '~/Dropbox/GeorgeMason/Grants/2020/HEI_Accountability/Data/pm_data/province_avg_ts.gg.png',
        province_avg_ts.gg,
        height = 12, width = 12, unit = 'in', scale = .8)








