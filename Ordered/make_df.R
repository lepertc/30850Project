##### Library #####
library(data.table)
library(lubridate)
library(readr)
library(stringr)

##### Load data #####
load("~/OneDrive/30850/PSETs/project/df.RData")
financials <- read_csv("~/OneDrive/30850/PSETs/project/30850Project/financials.csv")
weather <- read_csv("~/OneDrive/30850/PSETs/project/30850Project/Weather/NOAA_weather_data.csv")
stations <- read_csv("~/OneDrive/30850/PSETs/project/30850Project/stations.csv")

##### Functions #####
de.trend = function(df) {
  df$Freq.sqrt = sqrt(df$Freq)
  df$month = as.factor(month(df$Start.date))
  df$dow = as.factor(weekdays(df$Start.date))
  m = lm(Freq.sqrt ~ poly(Start.date, 2) + month + dow, df)
  df$fit = fitted(m)
  df$un.trended = df$Freq.sqrt - df$fit
  return(df)
}

de.trend.stations = function(df) {
  names(df) = c("Start.date", "station", "Freq")
  stations = as.list(levels(df$station))
  out = data.frame()
  for(i in stations) {
    print(i)
    if(!(i %in% c("Joliet St & MLK Ave SW/Bald Eagle Rec Ctr", 
                    "Lincoln Rd & Seaton Pl NE/Harry Thomas Rec Center"))) {
      df.i = df[df$station == i, ]
      df.i = de.trend(df.i)
      out = rbind(out, df.i)
    }
  }
  return(out)
}

convert.weather.date = function(date.str) {
  spl = str_split(date.str, "/")[[1]]
  if(nchar(spl[1]) == 1){
    spl[1] = paste("0", spl[1], sep = "")
  }
  if(nchar(spl[2]) == 1){
    spl[2] = paste("0", spl[2], sep = "")
  }
  date = as.Date(paste(spl[3], spl[1], spl[2], sep = "-"))
  return(date)
}


##### Executed Statements #####
df$count = 1
df = df[df$Start.date >= "2011-01-01", ]
daily.start = df[, list(Freq = sum(count)), by = list(Start.date, Start.station)]
daily.end = df[, list(Freq = sum(count)), by = list(Start.date, End.station)]
daily = df[, list(Freq = sum(count)), by = list(Start.date)]

daily = de.trend(daily)
daily.start = de.trend.stations(daily.start)
daily.end = de.trend.stations(daily.end)

financials$date = as.character(financials$date)
financials$Start.date = as.Date(paste(substr(financials$date, 0, 4), 
                                      substr(financials$date, 5, 6), 
                                      substr(financials$date, 7, 8), sep = "-"))
financials = financials[, c(3:11)]

daily.f = merge(daily, financials, by = "Start.date", all.x = T)
daily.start.f = merge(daily.start, financials, by = "Start.date", all.x = T)
daily.end.f= merge(daily.end, financials, by = "Start.date", all.x = T)

weather[is.na(weather) == TRUE] = 0

date.list = as.list(weather$DATE)
new.date.list = list()
for(i in 1:length(date.list)) {
  new.date.list[[i]] = convert.weather.date(date.list[[i]])
}

weather$date = as.Date(unlist(new.date.list), "1970-01-01")
weather = weather[, c(4:6, 8:28)]
names(weather)[24] = "Start.date"

daily.f = merge(daily.f, weather, by = "Start.date", all.x = T)
daily.start.f = merge(daily.start.f, weather, by = "Start.date", all.x = T)
daily.end.f= merge(daily.end.f, weather, by = "Start.date", all.x = T)

stations = stations[, c(3, 9:59)]
names(stations)[1] = "station"

daily.start.f = merge(daily.start.f, stations, by = "station", all.x = T)
daily.end.f= merge(daily.end.f, stations, by = "station", all.x = T)

