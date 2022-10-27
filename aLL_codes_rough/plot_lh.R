Bej_data_3 <- read.fst("Bej.fst", as.data.table = TRUE)
set.seed(1)

#train the data
#name the data
training <- Bej_data_3

testing <- copy( Bej_data_3)[, c("Temp_STM",
                                 "Td_STM", "Pres_STM", "Ws_STM",
                                 "Albedo_STM", "RH_STM", "PBL_STM",
                                 "SR_STM", "SM_STM") := 0]



test <- subset(testing, select =c("PM_STM", "ozone_STM", "Temp_STM",
                                  "Td_STM", "Pres_STM", "Ws_STM",
                                  "Albedo_STM", "RH_STM", "PBL_STM",
                                  "SR_STM", "SM_STM", "Date"))



#check the row numbers to ensure proper partitioning
nrow(Bej_data_3)
nrow(training)
nrow(testing)
summary(Bej_data_3)
summary(training)
summary(testing)


##Training the data with LM
summary( training)



# since we're not using any factor variables, we do want to keep the
# intercept in the model
lm_full <- lm(PM_STM ~ Temp_STM + Td_STM +
                Ws_STM +
                RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                SR_STM + SM_STM,
              data = training)



summary( lm_full)



# which are not NA in train?
not_na_pm <- which( !is.na( test$PM_STM))



#predicting
pred.lm <- predict(lm_full, newdata = test)[not_na_pm] + residuals( lm_full)
summary( residuals( lm_full))
summary( test$PM_STM)
pred.lm.num <- as.numeric(pred.lm)
pred <- as.data.table(pred.lm.num)




#plot using ggplot2
# create dataframe with actual and predicted values
plot_data <- data.frame(STM_no_met = pred.lm.num,  
                        STM_from_met_lm = test$PM_STM[not_na_pm] - pred.lm.num,
                        Time = training$Date[not_na_pm] )



# plot predicted values and actual values
ggplot(plot_data, aes(y = STM_no_met, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")



# plot portion attributable to met
ggplot(plot_data, aes(y = STM_from_met_lm, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")


write.fst( plot_data, "STM_met_no_met.fst")


