rm(list=ls())

setwd("~/UChicago/S18/Multiple Testing/Project")
load("allProcessed.RData")
rm(list=c("df"))
set.seed(06061995)

library(dplyr)
library(zoo)
library(lubridate)

#subsamples
days = unique(daily.f$Start.date)
N = length(days)
n = floor(.05*N)

s25 = sample(1:N,5*n)
s10 = sample(s25,2*n)
s5 = sample(s10, n)






# setup
names(daily.end.f)
names(daily.f)
names(daily.start.f)

#stations setup
daily.end = daily.end.f[, c("station", "Start.date", "Freq")]
daily.station = merge(daily.start.f, daily.end, by = c("station", "Start.date"),
                      all=TRUE)

#replace NAs
daily.station$Freq.x[is.na(daily.station$Freq.x)]=0
daily.station$Freq.y[is.na(daily.station$Freq.y)]=0

#total freq
daily.station = mutate(daily.station, Freq=Freq.x+Freq.y)
daily.station=daily.station[,(c(1:3,91:92,5:6,9:90))]
colnames(daily.station)[3:5]=c("Start","End","Total")

#impute weekend financials
library(zoo)
for (i in 8:15){  
  daily.f[,i] = na.locf(daily.f[,i],na.rm=FALSE)
  daily.f[,i] = na.locf(daily.f[,i],na.rm=FALSE,fromLast=TRUE)
}

#snow is factor
daily.f$SNOW = as.numeric(daily.f$SNOW>0)
#for (i in 21:38){
#  daily.f[,i] = as.factor(daily.f[,i])
#}

#lags
daily.f = mutate(daily.f, F.lag1=lag(Freq),F.lag2=lag(Freq,2),lag3=lag(Freq,3))

