#read data
Bej_data_whole <- fread("Bej_data.stm.csv")

#subset to remove irrelevant columns
Bej_data_raw <- subset(Bej_data_whole, select = c(Date, pm, ozone,
                                                  PM_STM, ozone_STM, 
                                            Temp_STM, Td_STM, Ws_STM,
                                            Wdfac, PRCPBool, RH_STM, PBL_STM, 
                                            Pres_STM, Albedo_STM, SR_STM, SM_STM,
                                            Cloud, dayf))


#plot using ggplot2
# create dataframe with actual and predicted values
plot_data_pm <- data.frame(PM2.5_raw = Bej_data_raw$pm,
                        PM2.5_STM = Bej_data_raw$PM_STM,
                        Time = Bej_data_raw$Date)
plot_data_pm$PM_res <- Bej_data_raw$pm - Bej_data_raw$PM_STM 

plot_data_pm.m <-  melt( plot_data_pm, id.vars = c('Time'))
plot_data_pm.m$Time <- as.Date( plot_data_pm.m$Time)


# plot predicted values and actual values
ggplot(plot_data_pm.m, aes(x = Time, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue", "black"))       
