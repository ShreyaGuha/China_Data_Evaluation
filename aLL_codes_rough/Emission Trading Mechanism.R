#Emission Trading Mechanism in China#

## try an sf object
library( sf)
library( data.table)
library( ggplot2)
library( maptools)
dir <- tempdir()


# download.file('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_CHN_1_sf.rds', 
#               file.path(dir, 'gadm36_CHN_1_sf.rds'))
# china_sf <- readRDS(file.path(dir, 'province.shp'))

# try this shapefile
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DBJ3BX

china_sf <-  st_read('province.shp')
plot( china_sf)

# check out the province names
china_sf$NAME_PINGY

#reading file
china_2011_pol <- fread("Emission.csv")

# check out our province names
china_2011_pol$Province
china_2011_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2011_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = china_2011_pol$Value)) + 
  scale_fill_binned() + 
  theme_bw() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  labs( title = "Provinces which implemented Emission Trading Mechanism")
