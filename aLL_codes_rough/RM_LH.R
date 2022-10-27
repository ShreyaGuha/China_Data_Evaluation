library( sf)
library( raster)
library( data.table)



## download china data
file1 <- 'https://wustl.box.com/shared/static/yjjdeop4fa2ntv4sxvubt1hnkxrvlu04.nc'

destfile.all <- 'D:\\'


download.file( file1, destfile = paste0(destfile.all, 'V4CH03_PM25_CHi_200001_200012-RH35.nc'))
file_1_downloaded <- paste0(destfile.all, 'V4CH03_PM25_CHi_200001_200012-RH35.nc')
ncin_raster <- raster( file_1_downloaded)