library(data.table)
library(ggplot2)
library(gridExtra)
load("useThis.RData")

daily.end = daily.end.f[, c("station", "Start.date", "Freq")]
daily.station = merge(daily.start.f, daily.end, by = c("station", "Start.date"))
daily.station$Freq.sqrt = sqrt(daily.station$Freq.x + daily.station$Freq.y)
daily.station$Freq.x = NULL
daily.station$Freq.y = NULL
# Intuition order

# Overall predictions
## Weather covariates in order 
## Financial covariates in order
daily.f = daily.f[, c(1:7, 16:20, 8:15, 21:38)]

# Start prediction
## Weather covariates in order 
## Financial covariates in order
## Census covariates in order
## Weather binaries

daily.station$MD = 1*(daily.station$state == "MD")
daily.station$VA = 1*(daily.station$state == "VA")
daily.station$state = NULL

daily.station$avg.household.size = gsub("-", NA, daily.station$avg.household.size)
daily.station$avg.household.size = as.numeric(daily.station$avg.household.size)

daily.station$mean.travel.time.work = gsub("N", NA, daily.station$mean.travel.time.work)
daily.station$mean.travel.time.work = as.numeric(daily.station$mean.travel.time.work)

daily.station$median.income = gsub("-", NA, daily.station$median.income)
daily.station$median.income = gsub("+", "", daily.station$median.income)
daily.station$median.income = as.numeric(daily.station$median.income)

daily.station$mean.income = gsub("-", NA, daily.station$mean.income)
daily.station$mean.income = gsub("+", "", daily.station$mean.income)
daily.station$mean.income = as.numeric(daily.station$mean.income)

daily.station$percent.families.below.poverty.line = gsub("-", NA, daily.station$percent.families.below.poverty.line)
daily.station$percent.families.below.poverty.line = as.numeric(daily.station$percent.families.below.poverty.line)

de.trend = function(df) {
  df$month = as.factor(month(df$Start.date))
  df$dow = as.factor(weekdays(df$Start.date))
  m = lm(Freq.sqrt ~ poly(Start.date, 2) + month + dow, df)
  df$fit = fitted(m)
  df$un.trended = df$Freq.sqrt - df$fit
  return(df)
}

daily.station = de.trend(daily.station)
