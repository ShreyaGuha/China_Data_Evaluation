

mydata <- rollingMean(mydata, pollutant = "o3", width = 8,
                      new.name = "rollingo3", data.thresh = 75)