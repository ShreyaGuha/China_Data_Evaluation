##Randall Martin data##
##Uneven legend axis##

#Year:2011#
RM2011.r <- brick("RM2011.nc")
plot(RM2011.r, main = "PM2.5 levels in 2011")

#Year:2012#
RM2012.r <- brick("RM2012.nc")
plot(RM2012.r, main = "PM2.5 levels in 2012")

#Year:2013#
RM2013.r <- brick("RM2013.nc")
plot(RM2013.r, main = "PM2.5 levels in 2013")

#Year:2014#
RM2014.r <- brick("RM2014.nc")
plot(RM2014.r, main = "PM2.5 levels in 2014")

#Year:2015#
RM2015.r <- brick("RM2015.nc")
plot(RM2015.r, main = "PM2.5 levels in 2015")

#Year:2016#
RM2016.r <- brick("RM2016.nc")
plot(RM2016.r, main = "PM2.5 levels in 2016")

#Year:2017#
RM2017.r <- brick("RM2017.nc")
plot(RM2017.r, main = "PM2.5 levels in 2017")

#Year:2018#
RM2018.r <- brick("RM2018.nc")
plot(RM2018.r, main = "PM2.5 levels in 2018")

RM2018.sf <- RM2018.r %>%
  rasterToPolygons() %>%
  st_as_sf()

RM2017.sf <- RM2017.r %>%
  rasterToPolygons() %>%
  st_as_sf()



