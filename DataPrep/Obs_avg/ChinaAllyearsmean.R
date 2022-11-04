##Annual Mean##

#set working directory same as the files
setwd("C:/Users/15712/Desktop/China_obs/New folder/Annual_Mean")

#list the files
data_files <- list.files("C:/Users/15712/Desktop/China_obs/New folder/Annual_Mean")

#Loop over file list importing them and binding them together
data_annual <- 
  lapply( data_files, fread, header = TRUE, stringsAsFactors = FALSE) %>%
  rbindlist(fill=TRUE)

#save data
fwrite(data_annual, file = "china_annual_mean_14_22.csv", row.names = F)

#remove n.NA column as it is not important for plotting
data_annual_plot <- subset(data_annual, 
                           select = -3)

#save data
fwrite(data_annual_plot, file = "china_mean_14_22.csv", row.names = F)
