#script to get daily weather data from NOAA_weather_data.csv
## See weather_types.txt for descriptions of weather types (WT01,...,WT22)
## Note: avergae temperatures only for 4/2011 onwards. Fix?

library(dplyr)
setwd("~/UChicago/S18/Multiple Testing/Project")
weather.df = read.csv("NOAA_weather_data.csv")
weather.df = weather.df[,-c(1,2)]

#make each weather type an indicator
for (i in 8:25){
  WT=weather.df[,i]
  weather.df[,i][is.na(WT)] <- 0
}

#make names human readable
col.names = c("Date","Avg.Wind.Speed","Percip","Snow",
              "Temp.Avg","Temp.Max","Temp.Min")
colnames(weather.df)[1:7] <- col.names

#Binary variable for snow
weather.df=mutate(weather.df, Snow.bin=(Snow>0))