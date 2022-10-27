##Processing .nc file for data comparison##
##Source: Randall-Martin (Atmospheric Composition Analysis Group)##
##China Regional Estimates (v4.ch.03)##

#installing library for running .nc files
install.packages("ncdf4") # for netcdf manipulation
install.packages("raster") #for raster manipulation
install.packages("sp") #required with sp
install.packages("rgdal") #for geospatial analysis
install.packages("ggplot2") #for plotting
library(ncdf4)
library(sp)
library(raster) 
library(rgdal) 
library(ggplot2) 

#open netcdf file (Randall-Martin file)
#sample year 2017
RM2017 <- nc_open("RM2017.nc")
print(RM2017) #reveals the file contents

##get coordinates
#get longitude
lon <- ncvar_get(RM2017,"LON")
nlon <- dim(lon)
head(lon)

#get latitude
lat <- ncvar_get(RM2017,"LAT")
nlat <- dim(lat)
head(lat)
print(c(nlon,nlat))

##getting variable PM2.5
pm_array <- ncvar_get(RM2017,"PM25")#store the data in a 3-d array
dim(pm_array) #shows the dimensions
#filling the attribute values as we know from "print"
dunits <- ncatt_get(RM2017,"PM25","units")
dlname <- ncatt_get(RM2017,"PM25","standard_name")
fillvalue <- ncatt_get(RM2017,"PM25","_FillValue")


#closing the nc file
nc_close(RM2017)

##Working with the .nc data##
#replacing missing or unavaible data with R's standard NA's
pm_array[pm_array == fillvalue$value] <- NA
#get one single slice or layer
pm_slice <- pm_array[72.015, 53.585] 
dim(pm_slice)

#quick map
image(lon,lat, pm_slice, col=rev(brewer.pal(10,"RdBu")))

