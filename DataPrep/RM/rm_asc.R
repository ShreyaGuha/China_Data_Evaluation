##Randall Martin Data, .asc format##
#Sample year: 2014#
library("tools") #using specific library functions
file.ext("RM_2014.asc") #check the file path extension
#it is an asc file, which is a text file

rm <- read.table("RM_2014.asc") #read file as table
write.csv(rm, file = "RM14.csv", row.names = F) #rewrite file in .csv format

rm14 <- read.csv("RM14.csv", header = FALSE, skip = 1) #open file in .csv format

#Data manipulation#
#replacing negative values with NA's
rm14$V1[rm14$V1 < 0] <- NA
rm14$V2[rm14$V2 < 0] <- NA
rm14[is.na(rm14)] = 0 #replacing negative values with NA's
rm14.2 <- rm14[rowSums(rm14 != 0) > 0, ] #retaining ONLY the non-zero rows
#converting the zeroes to negligble positive values
rm14.2$V1[rm14.2$V1 == 0] <- 0.01
rm14.2$V2[rm14.2$V2 == 0] <- 0.01

#generating rough spatial plot
library(sf)
points = st_as_sf(rm14.2, coords = c("V1", "V2"), crs = 4326) 
#crs refers to coordinate reference system, 
#crs is 4326 for WGS84, the projection of this data
#returns error stating missing values in coordinates
plot(st_geometry(points), pch=16, col="navy")

#simple basic plot
plot(rm14.2) 

