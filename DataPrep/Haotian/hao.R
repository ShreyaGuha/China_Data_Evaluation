#set working directory
hao <- setwd("C:/Users/15712/Desktop/China_obs/Haotian/home/jyq/aqdata/2012/beijing_pm25_20121009-20121221")
haotian <- list.files( hao, full.names = TRUE,
                       pattern = '.txt')
#loop and bind files
hao_2012 <- 
  lapply( haotian, fread, header = TRUE, stringsAsFactors = FALSE) %>%
  rbindlist

#save file
fwrite(hao_2012, file = "C:/Users/15712/Desktop/China_obs/Haotian/hao_yearly/hao_2012.csv",  
       row.names = F)

#set working directory
hao <- setwd("C:/Users/15712/Desktop/China_obs/Haotian/home/jyq/aqdata/2013/1")
haotian <- list.files( hao, full.names = TRUE)

#loop and bind files
hao_bind <- lapply(haotian, fread) %>% 
  rbindlist()

