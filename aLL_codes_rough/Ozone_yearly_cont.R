#For ozone
lm_comb_ozone <- lm(Ozone_STM ~ -1 + temp_STM * year +
                      prec_STM *year + rh_STM *year +
                      wind_STM * year + pbl_STM*year, data = Beijing_2011_18)
summary(lm_comb_ozone)

#extract coefficients
coef_lm_comb_ozone <- coef(lm_comb_ozone)

