# load the data.frame produced by getdata_bikeshare.R
library(data.table)
library(lubridate)

sample.df = function(df, n) {
  N = nrow(df)
  df$index = 1:nrow(df)
  keep = sample(N, n)
  df = subset(df, df$index %in% keep)
  df$index = NULL
  return(df)
}

aggregate.daily = function(df) {
  df$count <- 1
  daily <- df[, list(Mean.duration.seconds = mean(Duration.seconds), 
                     Freq = sum(count)),
              by = list(Start.date, Start.station, End.station, Member.type)]
  return(daily)
}

aggregate.daily.strong = function(df) {
  df$count <- 1
  daily <- df[, list(Mean.duration.seconds = mean(Duration.seconds), 
                     Freq = sum(count)),
              by = list(Start.date, Member.type)]
  return(daily)
}

aggregate.daily.strong.no.type = function(df) {
  df$count <- 1
  daily <- df[, list(Mean.duration.seconds = mean(Duration.seconds), 
                     Freq = sum(count)),
              by = list(Start.date)]
  return(daily)
}

aggregate.monthly.strong = function(df) {
  df$count <- 1
  df$month = month(df$Start.date)
  df$year = year(df$Start.date)
  daily <- df[, list(Mean.duration.seconds = mean(Duration.seconds), 
                     Freq = sum(count)),
              by = list(year, month, Member.type)]
  return(daily)
}

aggregate.hourly = function(df) {
  df$count <- 1
  hourly <- df[, list(Mean.duration.seconds = mean(Duration.seconds), 
                     Freq = sum(count)),
              by = list(Start.date, Start.station, End.station, Member.type, Start.hour)]
  return(hourly)
}

aggregate.hourly.strong = function(df) {
  df$count <- 1
  hourly <- df[, list(Mean.duration.seconds = mean(Duration.seconds), 
                      Freq = sum(count)),
               by = list(Start.date, Member.type, Start.hour)]
  return(hourly)
}


