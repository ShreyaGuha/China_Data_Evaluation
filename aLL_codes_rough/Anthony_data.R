###Anthony's Data: Example for 2013###
##Goal: Extract observed meteorological data for Beijing##

##Preparing the Data##
#using specific library functions
library("tools")
#getting and setting specific directory
getwd()
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/Data/china_isd_lite_2013")

#checking the type of file/ file extension
file_ext("450070-99999-2014.txt")
#it is a .txt file, in tabular format

#read data as data table
An_Be_13_1 <- read.table("450070-99999-2013")

#export data as a .csv file
write.csv(An_Be_13_1, file = "An_Be_13_1.csv", row.names = F)
