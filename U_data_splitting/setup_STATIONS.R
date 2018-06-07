rm(list=ls())

setwd("~/UChicago/S18/Multiple Testing/Project")
load("allProcessed.RData")
rm(list=c("df"))
set.seed(06061995)

library(dplyr)
library(lubridate)
library(glmnet)

# setup
names(daily.end.f)
names(daily.f)
names(daily.start.f)

#stations setup
daily.end = daily.end.f[, c("station", "Start.date", "Freq")]
daily.station = merge(daily.start.f, daily.end,
                      by = c("station", "Start.date"),all=TRUE)

daily.station$station = trimws(daily.station$station)

#stationa info
station.info = read.csv("30850Project/stations.csv")
station.info = station.info[-c(1,2,4,5,6)]
colnames(station.info)[1] = "station"

for (i in 5:54){
  station.info[,i]=as.numeric(station.info[,i])
}


#replace NAs
daily.station$Freq.x[is.na(daily.station$Freq.x)]=0
daily.station$Freq.y[is.na(daily.station$Freq.y)]=0

#total freq
daily.station = mutate(daily.station, Freq=Freq.x+Freq.y)
daily.station=daily.station[,(c(1:3,91:92,5:6,9:90))]
colnames(daily.station)[3:5]=c("Start","End","Total")

#2016/205
daily.station.16 = daily.station[year(daily.station$Start.date)==2016,]
daily.station.15 = daily.station[year(daily.station$Start.date)==2015,]

#filter out stations not used in Jan
jan.16 = unique(daily.station.16$station[daily.station$month==1])
daily.station.16 = filter(daily.station.16,
                          daily.station.16$station %in% as.factor(jan.16))

jan.15 = unique(daily.station.15$station[daily.station$month==1])
daily.station.15 = filter(daily.station.15,
                          daily.station.15$station %in% as.factor(jan.15))

#aggregate by station
gdf.16 = group_by(daily.station.16, station)
station.16.temp = summarise(gdf.16,Start=sum(Start),End=sum(End),Total=sum(Total)) 
station.16 = merge(station.16.temp,station.info)

gdf.15 = group_by(daily.station.15, station)
station.15.temp = summarise(gdf.15,Start=sum(Start),End=sum(End),Total=sum(Total)) 
station.15 = merge(station.15.temp,station.info)

#add population column
station.15 = mutate(station.15,
              pop = avg.household.size*number.of.households)
station.16 = mutate(station.16,
                    pop = avg.household.size*number.of.households)