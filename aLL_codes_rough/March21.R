###Plotting spatial maps for effectiveness of policy implementations in China###

#Prerequisites#
#installing libraries
library( sf)
library( data.table)
library( ggplot2)
library( maptools)
dir <- tempdir()

##For China spatial plots##

# download.file('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_CHN_1_sf.rds', 
#               file.path(dir, 'gadm36_CHN_1_sf.rds'))
# china_sf <- readRDS(file.path(dir, 'province.shp'))
# try this shapefile
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DBJ3BX

china_sf <-  st_read('province.shp') #for provinces
plot( china_sf)

# check out the province names
china_sf$NAME_PINGY


##1##
##plotting for PM2.5 emissions for different provinces##
##March 2021##

#reading file
china_2021 <- fread("March21.csv")

# check out our province names
china_2021$Province
china_2021$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2021, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = (china_2021$Value))) + 
  theme_bw() +
  scale_fill_binned() +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "PM2.5 Pollution in different provinces 
in China for March, 2021")

