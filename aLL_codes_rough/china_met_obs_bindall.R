#China Meteorology, all locations

#set working directory same as the files
setwd("C:/Users/15712/Desktop/China_Met/Sample")


#list the files
data_files <- list.files("C:/Users/15712/Desktop/China_Met/Sample")
data_files <- lapply(data_files, read.table) #read files

#set memory limit
memory.limit(size = 120000000)

#bind files
data <- data_files %>% reduce(full_join, by='V1')

#Error: cannot allocate vector of size 809.9 Gb











-------------
  
  

#Single File: Beijing#
#using specific library functions
library("tools")

#checking file extension

file_ext("450070-99999-2014.txt")
#it is a .txt file, in tabular format

#read data as data table
Bej_met_14 <- read.table("450070-99999-2014")

#export data as a .csv file
write.csv(Bej_met_14 , file = "Bej_met_14.csv", row.names = F)

#-----------------------
  
  
##Combine all files as .csv files

#set working directory same as the files
setwd("C:/Users/15712/Desktop/China_Met/china_isd_lite_2014")


#list the files
data_files <- list.files("C:/Users/15712/Desktop/China_Met/china_isd_lite_2014")

#Read the files
data_2014 <- lapply(data_files, fread)

#Create a list/rename
myList <- list(data_2014)
#Large list of 417 (what?)

#check the length & dimension of the list for looping
n <- length(myList)
#length is 1, dimension is zero

#Check for 1st element in list
myList[1]
#returns entire list

#For loop for writing as .csv file
for (i in 1:417 [[myList]]) {
  write.csv(myList[[i]], paste0(i,".csv"))
}

#not working, we need to fix the list!

#read newly saved files, use "list.files(pattern=*.csv)"
#bind the .csv files using loop, and write a new .csv file for a single year


   
  

