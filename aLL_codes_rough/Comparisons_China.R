##Processing .nc file for data comparison##
##Source: Randall-Martin (Atmospheric Composition Analysis Group)##
##China Regional Estimates (v4.ch.03)##
#Example Years: 2014 & 2017#

#installing library for running .nc files
install.packages("ncdf4") # for netcdf manipulation
install.packages("Rcpp") #for running raster & associated packages
install.packages("terra") #also for running raster
install.packages("raster")#for raster manipulation
install.packages("sf") #required with sp
install.packages("rgdal") #for geospatial analysis
install.packages("ggplot2") #for plotting


library(ncdf4)
library(sp)
library(Rcpp)
library(terra)
library(raster) 
library(rgdal) 
library(ggplot2) 



##example 1##
#sample year 2014
#open netcdf file (Randall-Martin file)
RM2014 <- nc_open("RM2014.nc")
print(RM2014) #reveals the file contents


RM2014.r <- brick("RM2014.nc")
plot(RM2014.r)

##Data Processing##
##get coordinates
#get longitude
lon <- ncvar_get(RM2014,"LON")
nlon <- dim(lon)
head(lon)

#get latitude
lat <- ncvar_get(RM2014,"LAT")
nlat <- dim(lat)
head(lat)
print(c(nlon,nlat))

t <- ncvar_get(RM2014, "time")

##getting variable PM2.5
pm_array <- ncvar_get(RM2014,"PM25") #store the data in a 3-d array
dim(pm_array) #shows the dimensions
#filling the attribute values as we know from "print"
dunits <- ncatt_get(RM2014,"PM25","units")
dlname <- ncatt_get(RM2014,"PM25","standard_name")
fillvalue <- ncatt_get(RM2014,"PM25","_FillValue")

# extract variable name, size and dimension
nc <- RM2014


v <- nc$var[[1]]
size <- v$varsize
dims <- v$ndims
nt <- size[dims]              # length of time dimension
lat <- nc$dim$latitude$vals   # latitude position
lon <- nc$dim$longitude$vals  # longitude position

# read PM25 variable
r<-list()
for (i in 1:nt) {
  start <- rep(1,dims)     # begin with start=(1,1,...,1)
  start[dims] <- i             # change to start=(1,1,...,i) to read    timestep i
  count <- size                # begin with count=(nx,ny,...,nt), reads entire var
  count[dims] <- 1             # change to count=(nx,ny,...,1) to read 1 tstep
  
  dt<-ncvar_get(nc, varid = 'PM25', start = start, count = count)
  
  # convert to raster
  r[i]<-raster(dt)
}

# create layer stack with time dimension
r<-stack(r)

# transpose the raster to have correct orientation
rt<-t(r)
extent(rt)<-extent(c(range(lon), range(lat)))

# plot the result
spplot(rt)

#closing the nc file
nc_close(RM2014)


##Working with the .nc data##
#replacing missing or unavaible data with R's standard NA's
pm_array[pm_array == fillvalue$value] <- NA
#get one single slice or layer
pm_slice <- pm_array[, 1] 
dim(pm_slice)

##Spatial Plot##
#quick map
image(lon,lat, pm_slice, col=rev(brewer.pal(10,"RdBu")))


##example 2##
#sample year 2017
#open netcdf file (Randall-Martin file)
RM2017 <- nc_open("RM2017.nc")
print(RM2017) #reveals the file contents


##Data Processing##
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



##Spatial Plot##
#quick map
image(lon,lat, pm_slice, col=rev(brewer.pal(10,"RdBu")))


