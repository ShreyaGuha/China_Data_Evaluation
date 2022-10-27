#Long-term contribution
lm_PM2.5_annual <- lm(kz_PM_annual ~ -1 + kz_temp_annual*year + kz_prec_annual*year + 
                        kz_RH_annual*year + kz_wind_annual*year + kz_pbl_annual*year, 
                      data = Beijing_2011_18)
summary(lm_PM2.5_annual) 
coef_lm_PM2.5_annual <- coef(lm_PM2.5_annual)

#Recreate yearly contributions
Beijing_2011 <- subset.data.frame(Beijing_2011_18, year == "11")
PM_temp_11 <- (coef_lm_PM2.5_annual["kz_temp_annual"] * Beijing_2011$TAVG) 
PM_RH_11 <- (coef_lm_PM2.5_annual["kz_RH_annual"] * Beijing_2011$RH) 
PM_wind_11 <- (coef_lm_PM2.5_annual["kz_wind_annual"] * Beijing_2011$wind) 
PM_prec_11 <- (coef_lm_PM2.5_annual["kz_prec_annual"] * Beijing_2011$PRCP) 
PM_pbl_11 <- (coef_lm_PM2.5_annual["kz_pbl_annual"] * Beijing_2011$PBL) 
PM_11 <- coef_lm_PM2.5_annual["year11"] + PM_temp_11 + PM_RH_11 + PM_wind_11 + PM_prec_11 + PM_pbl_11

Beijing_2012 <- subset.data.frame(Beijing_2011_18, year == "12")
PM_temp_12 <- (coef_lm_PM2.5_annual["kz_temp_annual"] * Beijing_2012$TAVG) + (coef_lm_PM2.5_annual["kz_temp_annual:year12"] * Beijing_2012$kz_temp_annual)
PM_RH_12 <- (coef_lm_PM2.5_annual["kz_RH_annual"] * Beijing_2012$RH) + (coef_lm_PM2.5_annual["year12:kz_RH_annual"] * Beijing_2012$kz_RH_annual)
PM_wind_12 <- (coef_lm_PM2.5_annual["kz_wind_annual"] * Beijing_2012$wind) + (coef_lm_PM2.5_annual["year12:kz_wind_annual"] * Beijing_2012$kz_wind_annual)
PM_prec_12 <- (coef_lm_PM2.5_annual["kz_prec_annual"] * Beijing_2012$PRCP) + (coef_lm_PM2.5_annual["year12:kz_prec_annual"] * Beijing_2012$kz_prec_annual)
PM_pbl_12 <- (coef_lm_PM2.5_annual["kz_pbl_annual"] * Beijing_2012$PBL) + (coef_lm_PM2.5_annual["year12:kz_pbl_annual"] * Beijing_2012$kz_pbl_annual)
PM_12 <- coef_lm_PM2.5_annual["year12"] + PM_temp_12 + PM_RH_12 + PM_wind_12 + PM_prec_12 + PM_pbl_12

Beijing_2013 <- subset.data.frame(Beijing_2011_18, year == "13")
PM_temp_13 <- (coef_lm_PM2.5_annual["kz_temp_annual"] * Beijing_2013$TAVG) + (coef_lm_PM2.5_annual["kz_temp_annual:year13"] * Beijing_2013$kz_temp_annual)
PM_RH_13 <- (coef_lm_PM2.5_annual["kz_RH_annual"] * Beijing_2013$RH) + (coef_lm_PM2.5_annual["year13:kz_RH_annual"] * Beijing_2013$kz_RH_annual)
PM_wind_13 <- (coef_lm_PM2.5_annual["kz_wind_annual"] * Beijing_2013$wind) + (coef_lm_PM2.5_annual["year13:kz_wind_annual"] * Beijing_2013$kz_wind_annual)
PM_prec_13 <- (coef_lm_PM2.5_annual["kz_prec_annual"] * Beijing_2013$PRCP) + (coef_lm_PM2.5_annual["year13:kz_prec_annual"] * Beijing_2013$kz_prec_annual)
PM_pbl_13 <- (coef_lm_PM2.5_annual["kz_pbl_annual"] * Beijing_2013$PBL) + (coef_lm_PM2.5_annual["year13:kz_pbl_annual"] * Beijing_2013$kz_pbl_annual)
PM_13 <- coef_lm_PM2.5_annual["year13"] + PM_temp_13 + PM_RH_13 + PM_wind_13 + PM_prec_13 + PM_pbl_13

Beijing_2014 <- subset.data.frame(Beijing_2011_18, year == "14")
PM_temp_14 <- (coef_lm_PM2.5_annual["kz_temp_annual"] * Beijing_2014$TAVG) + (coef_lm_PM2.5_annual["kz_temp_annual:year14"] * Beijing_2014$kz_temp_annual)
PM_RH_14 <- (coef_lm_PM2.5_annual["kz_RH_annual"] * Beijing_2014$RH) + (coef_lm_PM2.5_annual["year14:kz_RH_annual"] * Beijing_2014$kz_RH_annual)
PM_wind_14 <- (coef_lm_PM2.5_annual["kz_wind_annual"] * Beijing_2014$wind) + (coef_lm_PM2.5_annual["year14:kz_wind_annual"] * Beijing_2014$kz_wind_annual)
PM_prec_14 <- (coef_lm_PM2.5_annual["kz_prec_annual"] * Beijing_2014$PRCP) + (coef_lm_PM2.5_annual["year14:kz_prec_annual"] * Beijing_2014$kz_prec_annual)
PM_pbl_14 <- (coef_lm_PM2.5_annual["kz_pbl_annual"] * Beijing_2014$PBL) + (coef_lm_PM2.5_annual["year14:kz_pbl_annual"] * Beijing_2014$kz_pbl_annual)
PM_14 <- coef_lm_PM2.5_annual["year14"] + PM_temp_14 + PM_RH_14 + PM_wind_14 + PM_prec_14 + PM_pbl_14

Beijing_2015 <- subset.data.frame(Beijing_2011_18, year == "15")
PM_temp_15 <- (coef_lm_PM2.5_annual["kz_temp_annual"] * Beijing_2015$TAVG) + (coef_lm_PM2.5_annual["kz_temp_annual:year15"] * Beijing_2015$kz_temp_annual)
PM_RH_15 <- (coef_lm_PM2.5_annual["kz_RH_annual"] * Beijing_2015$RH) + (coef_lm_PM2.5_annual["year15:kz_RH_annual"] * Beijing_2015$kz_RH_annual)
PM_wind_15 <- (coef_lm_PM2.5_annual["kz_wind_annual"] * Beijing_2015$wind) + (coef_lm_PM2.5_annual["year15:kz_wind_annual"] * Beijing_2015$kz_wind_annual)
PM_prec_15 <- (coef_lm_PM2.5_annual["kz_prec_annual"] * Beijing_2015$PRCP) + (coef_lm_PM2.5_annual["year15:kz_prec_annual"] * Beijing_2015$kz_prec_annual)
PM_pbl_15 <- (coef_lm_PM2.5_annual["kz_pbl_annual"] * Beijing_2015$PBL) + (coef_lm_PM2.5_annual["year15:kz_pbl_annual"] * Beijing_2015$kz_pbl_annual)
PM_15 <- coef_lm_PM2.5_annual["year15"] + PM_temp_15 + PM_RH_15 + PM_wind_15 + PM_prec_15 + PM_pbl_15

Beijing_2016 <- subset.data.frame(Beijing_2011_18, year == "16")
PM_temp_16 <- (coef_lm_PM2.5_annual["kz_temp_annual"] * Beijing_2016$TAVG) + (coef_lm_PM2.5_annual["kz_temp_annual:year16"] * Beijing_2016$kz_temp_annual)
PM_RH_16 <- (coef_lm_PM2.5_annual["kz_RH_annual"] * Beijing_2016$RH) + (coef_lm_PM2.5_annual["year16:kz_RH_annual"] * Beijing_2016$kz_RH_annual)
PM_wind_16 <- (coef_lm_PM2.5_annual["kz_wind_annual"] * Beijing_2016$wind) + (coef_lm_PM2.5_annual["year16:kz_wind_annual"] * Beijing_2016$kz_wind_annual)
PM_prec_16 <- (coef_lm_PM2.5_annual["kz_prec_annual"] * Beijing_2016$PRCP) + (coef_lm_PM2.5_annual["year16:kz_prec_annual"] * Beijing_2016$kz_prec_annual)
PM_pbl_16 <- (coef_lm_PM2.5_annual["kz_pbl_annual"] * Beijing_2016$PBL) + (coef_lm_PM2.5_annual["year16:kz_pbl_annual"] * Beijing_2016$kz_pbl_annual)
PM_16 <- coef_lm_PM2.5_annual["year16"] + PM_temp_16 + PM_RH_16 + PM_wind_16 + PM_prec_16 + PM_pbl_16

Beijing_2017 <- subset.data.frame(Beijing_2011_18, year == "17")
PM_temp_17 <- (coef_lm_PM2.5_annual["kz_temp_annual"] * Beijing_2017$TAVG) + (coef_lm_PM2.5_annual["kz_temp_annual:year17"] * Beijing_2017$kz_temp_annual)
PM_RH_17 <- (coef_lm_PM2.5_annual["kz_RH_annual"] * Beijing_2017$RH) + (coef_lm_PM2.5_annual["year17:kz_RH_annual"] * Beijing_2017$kz_RH_annual)
PM_wind_17 <- (coef_lm_PM2.5_annual["kz_wind_annual"] * Beijing_2017$wind) + (coef_lm_PM2.5_annual["year17:kz_wind_annual"] * Beijing_2017$kz_wind_annual)
PM_prec_17 <- (coef_lm_PM2.5_annual["kz_prec_annual"] * Beijing_2017$PRCP) + (coef_lm_PM2.5_annual["year17:kz_prec_annual"] * Beijing_2017$kz_prec_annual)
PM_pbl_17 <- (coef_lm_PM2.5_annual["kz_pbl_annual"] * Beijing_2017$PBL) + (coef_lm_PM2.5_annual["year17:kz_pbl_annual"] * Beijing_2017$kz_pbl_annual)
PM_17 <- coef_lm_PM2.5_annual["year17"] + PM_temp_17 + PM_RH_17 + PM_wind_17 + PM_prec_17 + PM_pbl_17

Beijing_2018 <- subset.data.frame(Beijing_2011_18, year == "18")
PM_temp_18 <- (coef_lm_PM2.5_annual["kz_temp_annual"] * Beijing_2018$TAVG) + (coef_lm_PM2.5_annual["kz_temp_annual:year18"] * Beijing_2018$kz_temp_annual)
PM_RH_18 <- (coef_lm_PM2.5_annual["kz_RH_annual"] * Beijing_2018$RH) + (coef_lm_PM2.5_annual["year18:kz_RH_annual"] * Beijing_2018$kz_RH_annual)
PM_wind_18 <- (coef_lm_PM2.5_annual["kz_wind_annual"] * Beijing_2018$wind) + (coef_lm_PM2.5_annual["year18:kz_wind_annual"] * Beijing_2018$kz_wind_annual)
PM_prec_18 <- (coef_lm_PM2.5_annual["kz_prec_annual"] * Beijing_2018$PRCP) + (coef_lm_PM2.5_annual["year18:kz_prec_annual"] * Beijing_2018$kz_prec_annual)
PM_pbl_18 <- (coef_lm_PM2.5_annual["kz_pbl_annual"] * Beijing_2018$PBL) + (coef_lm_PM2.5_annual["year18:kz_pbl_annual"] * Beijing_2018$kz_pbl_annual)
PM_18 <- coef_lm_PM2.5_annual["year18"] + PM_temp_18 + PM_RH_18 + PM_wind_18 + PM_prec_18 + PM_pbl_18

#plot yearly contributions together
PM_met_ann <- rbind(c(PM_11, PM_12, PM_13, PM_14, PM_15, PM_16, PM_17, PM_18))
PM_met_ann <- t(PM_met_ann)
PM_met_ann <- as.data.table(PM_met_ann)

PM_met_ann$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
PM_met_ann$year <- as.factor(format(PM_met_ann$date, "%y"))

PM_met_ann.melt <- melt(PM_met_ann, id.vars = c('date', 'year'))
PM_met_ann.melt$date <- as.Date( PM_met_ann.melt$date)
PM_met_ann.melt$year <- as.factor(format(PM_met_ann.melt$date, "%y"))

theme_set(theme_bw())
ggplot( PM_met_ann.melt) + (aes( x = date, y = value,
                                 group = variable, color = variable)) +
  geom_line() +
  xlab("time(years)") +
  ylab("PM2.5 detrended concentrations (ug/cc)") +
  ggtitle("PM2.5 annually detrended concentrations due to meteorological influences") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.title = element_blank(), legend.position = "none")



#Seasonal contribution
lm_comb_seas <- lm(kz_PM_seasonal ~ -1 + kz_temp_seasonal *year + kz_prec_seasonal *year
                   + kz_rh_seasonal *year + kz_wind_seasonal *year + kz_pbl_seasonal *year,
                   data = Beijing_2011_18)
summary(lm_comb_seas)
coef_lm_comb <- coef(lm_comb_seas)

#Recreate yearly contributions
Beijing_2011 <- subset.data.frame(Beijing_2011_18, year == "11")
PM_temp_11 <- (coef_lm_comb["kz_temp_seasonal"] * Beijing_2011$TAVG)
PM_RH_11 <- (coef_lm_comb["kz_rh_seasonal"] * Beijing_2011$RH)
PM_wind_11 <- (coef_lm_comb["kz_wind_seasonal"] * Beijing_2011$wind)
PM_prec_11 <- (coef_lm_comb["kz_prec_seasonal"] * Beijing_2011$PRCP)
PM_pbl_11 <- (coef_lm_comb["kz_pbl_seasonal"] * Beijing_2011$PBL)
PM_11 <- coef_lm_comb["year11"] + PM_temp_11 + PM_RH_11 + PM_wind_11 + PM_prec_11 + PM_pbl_11

Beijing_2012 <- subset.data.frame(Beijing_2011_18, year == "12")
PM_temp_12 <- (coef_lm_comb["kz_temp_seasonal"] * Beijing_2012$TAVG) + (coef_lm_comb["kz_temp_seasonal:year12"] * Beijing_2012$kz_temp_seasonal)
PM_RH_12 <- (coef_lm_comb["kz_rh_seasonal"] * Beijing_2012$RH) + (coef_lm_comb["year12:kz_rh_seasonal"] * Beijing_2012$kz_rh_seasonal)
PM_wind_12 <- (coef_lm_comb["kz_wind_seasonal"] * Beijing_2012$wind) + (coef_lm_comb["year12:kz_wind_seasonal"] * Beijing_2012$kz_wind_seasonal)
PM_prec_12 <- (coef_lm_comb["kz_prec_seasonal"] * Beijing_2012$PRCP) + (coef_lm_comb["year12:kz_prec_seasonal"] * Beijing_2012$kz_prec_seasonal)
PM_pbl_12 <- (coef_lm_comb["kz_pbl_seasonal"] * Beijing_2012$PBL) + (coef_lm_comb["year12:kz_pbl_seasonal"] * Beijing_2012$kz_pbl_seasonal)
PM_12 <- coef_lm_comb["year12"] + PM_temp_12 + PM_RH_12 + PM_wind_12 + PM_prec_12 + PM_pbl_12

Beijing_2013 <- subset.data.frame(Beijing_2011_18, year == "13")
PM_temp_13 <- (coef_lm_comb["kz_temp_seasonal"] * Beijing_2013$TAVG) + (coef_lm_comb["kz_temp_seasonal:year13"] * Beijing_2013$kz_temp_seasonal)
PM_RH_13 <- (coef_lm_comb["kz_rh_seasonal"] * Beijing_2013$RH) + (coef_lm_comb["year13:kz_rh_seasonal"] * Beijing_2013$kz_rh_seasonal)
PM_wind_13 <- (coef_lm_comb["kz_wind_seasonal"] * Beijing_2013$wind) + (coef_lm_comb["year13:kz_wind_seasonal"] * Beijing_2013$kz_wind_seasonal)
PM_prec_13 <- (coef_lm_comb["kz_prec_seasonal"] * Beijing_2013$PRCP) + (coef_lm_comb["year13:kz_prec_seasonal"] * Beijing_2013$kz_prec_seasonal)
PM_pbl_13 <- (coef_lm_comb["kz_pbl_seasonal"] * Beijing_2013$PBL) + (coef_lm_comb["year13:kz_pbl_seasonal"] * Beijing_2013$kz_pbl_seasonal)
PM_13 <- coef_lm_comb["year13"] + PM_temp_13 + PM_RH_13 + PM_wind_13 + PM_prec_13 + PM_pbl_13

Beijing_2014 <- subset.data.frame(Beijing_2011_18, year == "14")
PM_temp_14 <- (coef_lm_comb["kz_temp_seasonal"] * Beijing_2014$TAVG) + (coef_lm_comb["kz_temp_seasonal:year14"] * Beijing_2014$kz_temp_seasonal)
PM_RH_14 <- (coef_lm_comb["kz_rh_seasonal"] * Beijing_2014$RH) + (coef_lm_comb["year14:kz_rh_seasonal"] * Beijing_2014$kz_rh_seasonal)
PM_wind_14 <- (coef_lm_comb["kz_wind_seasonal"] * Beijing_2014$wind) + (coef_lm_comb["year14:kz_wind_seasonal"] * Beijing_2014$kz_wind_seasonal)
PM_prec_14 <- (coef_lm_comb["kz_prec_seasonal"] * Beijing_2014$PRCP) + (coef_lm_comb["year14:kz_prec_seasonal"] * Beijing_2014$kz_prec_seasonal)
PM_pbl_14 <- (coef_lm_comb["kz_pbl_seasonal"] * Beijing_2014$PBL) + (coef_lm_comb["year14:kz_pbl_seasonal"] * Beijing_2014$kz_pbl_seasonal)
PM_14 <- coef_lm_comb["year14"] + PM_temp_14 + PM_RH_14 + PM_wind_14 + PM_prec_14 + PM_pbl_14

Beijing_2015 <- subset.data.frame(Beijing_2011_18, year == "15")
PM_temp_15 <- (coef_lm_comb["kz_temp_seasonal"] * Beijing_2015$TAVG) + (coef_lm_comb["kz_temp_seasonal:year15"] * Beijing_2015$kz_temp_seasonal)
PM_RH_15 <- (coef_lm_comb["kz_rh_seasonal"] * Beijing_2015$RH) + (coef_lm_comb["year15:kz_rh_seasonal"] * Beijing_2015$kz_rh_seasonal)
PM_wind_15 <- (coef_lm_comb["kz_wind_seasonal"] * Beijing_2015$wind) + (coef_lm_comb["year15:kz_wind_seasonal"] * Beijing_2015$kz_wind_seasonal)
PM_prec_15 <- (coef_lm_comb["kz_prec_seasonal"] * Beijing_2015$PRCP) + (coef_lm_comb["year15:kz_prec_seasonal"] * Beijing_2015$kz_prec_seasonal)
PM_pbl_15 <- (coef_lm_comb["kz_pbl_seasonal"] * Beijing_2015$PBL) + (coef_lm_comb["year15:kz_pbl_seasonal"] * Beijing_2015$kz_pbl_seasonal)
PM_15 <- coef_lm_comb["year15"] + PM_temp_15 + PM_RH_15 + PM_wind_15 + PM_prec_15 + PM_pbl_15

Beijing_2016 <- subset.data.frame(Beijing_2011_18, year == "16")
PM_temp_16 <- (coef_lm_comb["kz_temp_seasonal"] * Beijing_2016$TAVG) + (coef_lm_comb["kz_temp_seasonal:year16"] * Beijing_2016$kz_temp_seasonal)
PM_RH_16 <- (coef_lm_comb["kz_rh_seasonal"] * Beijing_2016$RH) + (coef_lm_comb["year16:kz_rh_seasonal"] * Beijing_2016$kz_rh_seasonal)
PM_wind_16 <- (coef_lm_comb["kz_wind_seasonal"] * Beijing_2016$wind) + (coef_lm_comb["year16:kz_wind_seasonal"] * Beijing_2016$kz_wind_seasonal)
PM_prec_16 <- (coef_lm_comb["kz_prec_seasonal"] * Beijing_2016$PRCP) + (coef_lm_comb["year16:kz_prec_seasonal"] * Beijing_2016$kz_prec_seasonal)
PM_pbl_16 <- (coef_lm_comb["kz_pbl_seasonal"] * Beijing_2016$PBL) + (coef_lm_comb["year16:kz_pbl_seasonal"] * Beijing_2016$kz_pbl_seasonal)
PM_16 <- coef_lm_comb["year16"] + PM_temp_16 + PM_RH_16 + PM_wind_16 + PM_prec_16 + PM_pbl_16

Beijing_2017 <- subset.data.frame(Beijing_2011_18, year == "17")
PM_temp_17 <- (coef_lm_comb["kz_temp_seasonal"] * Beijing_2017$TAVG) + (coef_lm_comb["kz_temp_seasonal:year17"] * Beijing_2017$kz_temp_seasonal)
PM_RH_17 <- (coef_lm_comb["kz_rh_seasonal"] * Beijing_2017$RH) + (coef_lm_comb["year17:kz_rh_seasonal"] * Beijing_2017$kz_rh_seasonal)
PM_wind_17 <- (coef_lm_comb["kz_wind_seasonal"] * Beijing_2017$wind) + (coef_lm_comb["year17:kz_wind_seasonal"] * Beijing_2017$kz_wind_seasonal)
PM_prec_17 <- (coef_lm_comb["kz_prec_seasonal"] * Beijing_2017$PRCP) + (coef_lm_comb["year17:kz_prec_seasonal"] * Beijing_2017$kz_prec_seasonal)
PM_pbl_17 <- (coef_lm_comb["kz_pbl_seasonal"] * Beijing_2017$PBL) + (coef_lm_comb["year17:kz_pbl_seasonal"] * Beijing_2017$kz_pbl_seasonal)
PM_17 <- coef_lm_comb["year17"] + PM_temp_17 + PM_RH_17 + PM_wind_17 + PM_prec_17 + PM_pbl_17

Beijing_2018 <- subset.data.frame(Beijing_2011_18, year == "18")
PM_temp_18 <- (coef_lm_comb["kz_temp_seasonal"] * Beijing_2018$TAVG) + (coef_lm_comb["kz_temp_seasonal:year18"] * Beijing_2018$kz_temp_seasonal)
PM_RH_18 <- (coef_lm_comb["kz_rh_seasonal"] * Beijing_2018$RH) + (coef_lm_comb["year18:kz_rh_seasonal"] * Beijing_2018$kz_rh_seasonal)
PM_wind_18 <- (coef_lm_comb["kz_wind_seasonal"] * Beijing_2018$wind) + (coef_lm_comb["year18:kz_wind_seasonal"] * Beijing_2018$kz_wind_seasonal)
PM_prec_18 <- (coef_lm_comb["kz_prec_seasonal"] * Beijing_2018$PRCP) + (coef_lm_comb["year18:kz_prec_seasonal"] * Beijing_2018$kz_prec_seasonal)
PM_pbl_18 <- (coef_lm_comb["kz_pbl_seasonal"] * Beijing_2018$PBL) + (coef_lm_comb["year18:kz_pbl_seasonal"] * Beijing_2018$kz_pbl_seasonal)
PM_18 <- coef_lm_comb["year18"] + PM_temp_18 + PM_RH_18 + PM_wind_18 + PM_prec_18 + PM_pbl_18

#plot yearly contributions together
PM_met_seas <- rbind(c(PM_11, PM_12, PM_13, PM_14, PM_15, PM_16, PM_17, PM_18))
PM_met_seas <- t(PM_met_seas)
PM_met_seas <- as.data.table(PM_met_seas)

PM_met_seas$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
PM_met_seas$year <- as.factor(format(PM_met_seas$date, "%y"))

PM_met_seas.melt <- melt(PM_met_seas, id.vars = c('date', 'year'))
PM_met_seas.melt$date <- as.Date( PM_met_seas.melt$date)
PM_met_seas.melt$year <- as.factor(format(PM_met_seas.melt$date, "%y"))

theme_set(theme_bw())
ggplot( PM_met_seas.melt) + (aes( x = date, y = value,
                                  group = variable, color = variable)) +
  geom_line() +
  xlab("time(years)") +
  ylab("PM2.5 detrended concentrations (ug/cc)") +
  ggtitle("PM2.5 seasonally detrended concentrations due to meteorological influences") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.title = element_blank(), legend.position = "none")


#Short-term contribution
lm_comb <- lm(PM_STM ~ -1 + temp_STM * year +
                prec_STM *year + rh_STM *year +
                wind_STM * year + pbl_STM*year, data = Beijing_2011_18)
summary(lm_comb)

#extract coefficients
coef_lm_comb <- coef(lm_comb)

#Recreate yearly contributions
Beijing_2011 <- subset.data.frame(Beijing_2011_18, year == "11")
PM_temp_11 <- (coef_lm_comb["temp_STM"] * Beijing_2011$TAVG) 
PM_RH_11 <- (coef_lm_comb["rh_STM"] * Beijing_2011$RH) 
PM_wind_11 <- (coef_lm_comb["wind_STM"] * Beijing_2011$wind) 
PM_prec_11 <- (coef_lm_comb["prec_STM"] * Beijing_2011$PRCP) 
PM_pbl_11 <- (coef_lm_comb["pbl_STM"] * Beijing_2011$PBL) 
PM_11 <- coef_lm_comb["year11"] + PM_temp_11 + PM_RH_11 + PM_wind_11 + PM_prec_11 + PM_pbl_11

Beijing_2012 <- subset.data.frame(Beijing_2011_18, year == "12")
PM_temp_12 <- (coef_lm_comb["temp_STM"] * Beijing_2012$TAVG) + (coef_lm_comb["temp_STM:year12"] * Beijing_2012$temp_STM)
PM_RH_12 <- (coef_lm_comb["rh_STM"] * Beijing_2012$RH) + (coef_lm_comb["year12:rh_STM"] * Beijing_2012$rh_STM)
PM_wind_12 <- (coef_lm_comb["wind_STM"] * Beijing_2012$wind) + (coef_lm_comb["year12:wind_STM"] * Beijing_2012$wind_STM)
PM_prec_12 <- (coef_lm_comb["prec_STM"] * Beijing_2012$PRCP) + (coef_lm_comb["year12:prec_STM"] * Beijing_2012$prec_STM)
PM_pbl_12 <- (coef_lm_comb["pbl_STM"] * Beijing_2012$PBL) + (coef_lm_comb["year12:pbl_STM"] * Beijing_2012$pbl_STM)
PM_12 <- coef_lm_comb["year12"] + PM_temp_12 + PM_RH_12 + PM_wind_12 + PM_prec_12 + PM_pbl_12

Beijing_2013 <- subset.data.frame(Beijing_2011_18, year == "13")
PM_temp_13 <- (coef_lm_comb["temp_STM"] * Beijing_2013$TAVG) + (coef_lm_comb["temp_STM:year13"] * Beijing_2013$temp_STM)
PM_RH_13 <- (coef_lm_comb["rh_STM"] * Beijing_2013$RH) + (coef_lm_comb["year13:rh_STM"] * Beijing_2013$rh_STM)
PM_wind_13 <- (coef_lm_comb["wind_STM"] * Beijing_2013$wind) + (coef_lm_comb["year13:wind_STM"] * Beijing_2013$wind_STM)
PM_prec_13 <- (coef_lm_comb["prec_STM"] * Beijing_2013$PRCP) + (coef_lm_comb["year13:prec_STM"] * Beijing_2013$prec_STM)
PM_pbl_13 <- (coef_lm_comb["pbl_STM"] * Beijing_2013$PBL) + (coef_lm_comb["year13:pbl_STM"] * Beijing_2013$pbl_STM)
PM_13 <- coef_lm_comb["year13"] + PM_temp_13 + PM_RH_13 + PM_wind_13 + PM_prec_13 + PM_pbl_13

Beijing_2014 <- subset.data.frame(Beijing_2011_18, year == "14")
PM_temp_14 <- (coef_lm_comb["temp_STM"] * Beijing_2014$TAVG) + (coef_lm_comb["temp_STM:year14"] * Beijing_2014$temp_STM)
PM_RH_14 <- (coef_lm_comb["rh_STM"] * Beijing_2014$RH) + (coef_lm_comb["year14:rh_STM"] * Beijing_2014$rh_STM)
PM_wind_14 <- (coef_lm_comb["wind_STM"] * Beijing_2014$wind) + (coef_lm_comb["year14:wind_STM"] * Beijing_2014$wind_STM)
PM_prec_14 <- (coef_lm_comb["prec_STM"] * Beijing_2014$PRCP) + (coef_lm_comb["year14:prec_STM"] * Beijing_2014$prec_STM)
PM_pbl_14 <- (coef_lm_comb["pbl_STM"] * Beijing_2014$PBL) + (coef_lm_comb["year14:pbl_STM"] * Beijing_2014$pbl_STM)
PM_14 <- coef_lm_comb["year14"] + PM_temp_14 + PM_RH_14 + PM_wind_14 + PM_prec_14 + PM_pbl_14

Beijing_2015 <- subset.data.frame(Beijing_2011_18, year == "15")
PM_temp_15 <- (coef_lm_comb["temp_STM"] * Beijing_2015$TAVG) + (coef_lm_comb["temp_STM:year15"] * Beijing_2015$temp_STM)
PM_RH_15 <- (coef_lm_comb["rh_STM"] * Beijing_2015$RH) + (coef_lm_comb["year15:rh_STM"] * Beijing_2015$rh_STM)
PM_wind_15 <- (coef_lm_comb["wind_STM"] * Beijing_2015$wind) + (coef_lm_comb["year15:wind_STM"] * Beijing_2015$wind_STM)
PM_prec_15 <- (coef_lm_comb["prec_STM"] * Beijing_2015$PRCP) + (coef_lm_comb["year15:prec_STM"] * Beijing_2015$prec_STM)
PM_pbl_15 <- (coef_lm_comb["pbl_STM"] * Beijing_2015$PBL) + (coef_lm_comb["year15:pbl_STM"] * Beijing_2015$pbl_STM)
PM_15 <- coef_lm_comb["year15"] + PM_temp_15 + PM_RH_15 + PM_wind_15 + PM_prec_15 + PM_pbl_15

Beijing_2016 <- subset.data.frame(Beijing_2011_18, year == "16")
PM_temp_16 <- (coef_lm_comb["temp_STM"] * Beijing_2016$TAVG) + (coef_lm_comb["temp_STM:year16"] * Beijing_2016$temp_STM)
PM_RH_16 <- (coef_lm_comb["rh_STM"] * Beijing_2016$RH) + (coef_lm_comb["year16:rh_STM"] * Beijing_2016$rh_STM)
PM_wind_16 <- (coef_lm_comb["wind_STM"] * Beijing_2016$wind) + (coef_lm_comb["year16:wind_STM"] * Beijing_2016$wind_STM)
PM_prec_16 <- (coef_lm_comb["prec_STM"] * Beijing_2016$PRCP) + (coef_lm_comb["year16:prec_STM"] * Beijing_2016$prec_STM)
PM_pbl_16 <- (coef_lm_comb["pbl_STM"] * Beijing_2016$PBL) + (coef_lm_comb["year16:pbl_STM"] * Beijing_2016$pbl_STM)
PM_16 <- coef_lm_comb["year16"] + PM_temp_16 + PM_RH_16 + PM_wind_16 + PM_prec_16 + PM_pbl_16

Beijing_2017 <- subset.data.frame(Beijing_2011_18, year == "17")
PM_temp_17 <- (coef_lm_comb["temp_STM"] * Beijing_2017$TAVG) + (coef_lm_comb["temp_STM:year17"] * Beijing_2017$temp_STM)
PM_RH_17 <- (coef_lm_comb["rh_STM"] * Beijing_2017$RH) + (coef_lm_comb["year17:rh_STM"] * Beijing_2017$rh_STM)
PM_wind_17 <- (coef_lm_comb["wind_STM"] * Beijing_2017$wind) + (coef_lm_comb["year17:wind_STM"] * Beijing_2017$wind_STM)
PM_prec_17 <- (coef_lm_comb["prec_STM"] * Beijing_2017$PRCP) + (coef_lm_comb["year17:prec_STM"] * Beijing_2017$prec_STM)
PM_pbl_17 <- (coef_lm_comb["pbl_STM"] * Beijing_2017$PBL) + (coef_lm_comb["year17:pbl_STM"] * Beijing_2017$pbl_STM)
PM_17 <- coef_lm_comb["year17"] + PM_temp_17 + PM_RH_17 + PM_wind_17 + PM_prec_17 + PM_pbl_17

Beijing_2018 <- subset.data.frame(Beijing_2011_18, year == "18")
PM_temp_18 <- (coef_lm_comb["temp_STM"] * Beijing_2018$TAVG) + (coef_lm_comb["temp_STM:year18"] * Beijing_2018$temp_STM)
PM_RH_18 <- (coef_lm_comb["rh_STM"] * Beijing_2018$RH) + (coef_lm_comb["year18:rh_STM"] * Beijing_2018$rh_STM)
PM_wind_18 <- (coef_lm_comb["wind_STM"] * Beijing_2018$wind) + (coef_lm_comb["year18:wind_STM"] * Beijing_2018$wind_STM)
PM_prec_18 <- (coef_lm_comb["prec_STM"] * Beijing_2018$PRCP) + (coef_lm_comb["year18:prec_STM"] * Beijing_2018$prec_STM)
PM_pbl_18 <- (coef_lm_comb["pbl_STM"] * Beijing_2018$PBL) + (coef_lm_comb["year18:pbl_STM"] * Beijing_2018$pbl_STM)
PM_18 <- coef_lm_comb["year18"] + PM_temp_18 + PM_RH_18 + PM_wind_18 + PM_prec_18 + PM_pbl_18

#plot yearly contributions together
PM_met <- rbind(c(PM_11, PM_12, PM_13, PM_14, PM_15, PM_16, PM_17, PM_18))
PM_met <- t(PM_met)
PM_met <- as.data.table(PM_met)

PM_met$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
PM_met$year <- as.factor(format(PM_met$date, "%y"))

PM_met.melt <- melt(PM_met, id.vars = c('date', 'year'))
PM_met.melt$date <- as.Date( PM_met.melt$date)
PM_met.melt$year <- as.factor(format(PM_met.melt$date, "%y"))


theme_set(theme_bw())
ggplot( PM_met.melt) + (aes( x = date, y = value,
                             group = variable, color = variable)) +
  geom_line() +
  xlab("time(years)") +
  ylab("PM2.5 detrended concentrations (ug/cc)") +
  ggtitle("PM2.5 detrended concentrations due to short-term meteorological influences") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.title = element_blank(), legend.position = "none") 


#calculating the residuals
PM_res <- Beijing_2011_18$PM2.5_avg_conc - PM_met.melt$value - PM_met_seas.melt$value - PM_met_ann.melt$value
PM_res <- as.data.table(PM_res)
PM_res$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
PM_res$year <- as.factor(format(PM_res$date, "%y"))

PM_res.melt <- melt(PM_res, id.vars = c('date', 'year'))
PM_res.melt$date <- as.Date( PM_res.melt$date)

theme_set(theme_bw())
ggplot( PM_res.melt) + (aes( x = date, y = value,
                             group = variable, color = variable)) +
  geom_line() +
  xlab("time(years)") +
  ylab("PM2.5 residual concentrations (ug/cc)") +
  ggtitle("PM2.5 residual concentrations due to emission changes") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.title = element_blank(), legend.position = "none") 


#plotting
ggplot() + 
  geom_line(data = Beijing_2011_18, aes(x = date, y = PM2.5_avg_conc, color = "grey")) +
  geom_line(data = PM_res, aes(x = date, y = PM_res), color ="black") +
  ggtitle("Raw & Meteorologically detrended PM2.5 concentrations") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  xlab('time(years)') +
  ylab('PM2.5 concentration(ug/cc)') +
  scale_color_manual(name = "Trend", values = c("Observed" = "grey", 
                                                "All removed" = "black"))


#calculating the residuals with model values
PM_res <- PM_met_raw.melt$value - PM_met.melt$value - 
  PM_met_seas.melt$value - PM_met_ann.melt$value
PM_res <- as.data.table(PM_res)
PM_res$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
PM_res$year <- as.factor(format(PM_res$date, "%y"))

PM_res.melt <- melt(PM_res, id.vars = c('date', 'year'))
PM_res.melt$date <- as.Date( PM_res.melt$date)

theme_set(theme_bw())
ggplot( PM_res.melt) + (aes( x = date, y = value,
                             group = variable, color = variable)) +
  geom_line() +
  xlab("time(years)") +
  ylab("PM2.5 residual concentrations (ug/cc)") +
  ggtitle("PM2.5 residual concentrations due to emission changes") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.title = element_blank(), legend.position = "none") 


#Everything together
ggplot() + 
  geom_line(data = PM_met_raw.melt, aes(x = date, y = value, color = "grey")) +
  geom_line(data = PM_res.melt, aes(x = date, y = value), color ="black") +
  ggtitle("Raw & Meteorologically detrended PM2.5 concentrations") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  xlab('time(years)') +
  ylab('PM2.5 concentration(ug/cc)') +
  scale_color_manual(name = "Trend", values = c("Observed" = "grey", 
                                                "All removed" = "black"))

ggplot() + 
  theme_grey() +
  geom_line(data = PM_met_raw.melt, aes(x = as.numeric(year), y = value, color = "grey98")) +
  geom_line(data = PM_res.melt, aes(x = as.numeric(year), y = value), color ="black") +
  ggtitle("Raw & Meteorologically detrended PM2.5 concentrations") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  xlab('time(years)') +
  ylab('PM2.5 concentration(ug/cc)') +
  scale_color_manual(name = "Trend", values = c("Observed" = "grey98", 
                                                "STM removed" = "black"))


#Everything by year
PM_raw_year <-aggregate.data.frame(PM_met_raw.melt$value, list(PM_met_raw.melt$year), 
                                   FUN = mean)
PM_raw_year$year <- PM_raw_year$Group.1
PM_raw_year$PM <- PM_raw_year$x
PM_res_year <- aggregate.data.frame(PM_res.melt$value, list(PM_res.melt$year), 
                                    FUN = mean)
PM_res_year$year <- PM_res_year$Group.1
PM_res_year$PM <- PM_res_year$x


#plotting
ggplot() + 
  geom_line(data = PM_raw_year, aes(x = as.numeric(year), y = PM, color = "grey")) +
  geom_line(data = PM_res_year, aes(x = as.numeric(year), y = PM), color ="black") +
  ggtitle("Raw & Meteorologically detrended PM2.5 concentrations") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  xlab('time(years)') +
  ylab('PM2.5 concentration(ug/cc)') +
  scale_color_manual(name = "Trend", values = c("Observed" = "grey", 
                                                "All removed" = "black"))


ggplot() + 
  geom_line(data = PM_met_raw.melt, aes(x = date, y = value, color = "grey")) +
  geom_line(data = PM_res.melt, aes(x = date, y = value), color ="black") +
  ggtitle("Raw & Meteorologically detrended PM2.5 concentrations") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  xlab('time(years)') +
  ylab('PM2.5 concentration(ug/cc)') +
  scale_color_manual(name = "Trend", values = c("Observed" = "grey", 
                                                "All removed" = "black"))



