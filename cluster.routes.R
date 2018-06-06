library(data.table)

df$count = 1
hourly.station <- df[, list(Freq = sum(count)), 
                     by = list(Start.station, End.station, Start.hour)]

station = spread(hourly.station, Start.hour, Freq)

station[is.na(station) == TRUE] = 0
station$S = station$`0` + station$`1` + station$`2` + station$`3` + station$`4` +
  station$`5` + station$`6` + station$`7` + station$`8` + station$`9` + station$`10` +
  station$`11` + station$`12` + station$`12` + station$`13` + station$`14` + 
  station$`15` + station$`16` + station$`17` + station$`18` + station$`19` + 
  station$`20` + station$`21` + station$`22` + station$`23`
normed = station[, 3:26]/station$S
station.norm = station
station.norm[, 3:26] = normed
row.names(station.norm) = paste(station.norm$Start.station, station.norm$End.station, 
                                sep = " - ")
station.norm = station.norm[, 3:26]

set.seed(19930321)
station.cluster <- kmeans(station.norm, 5)

cent = as.data.frame(t(station.cluster$centers))
cent$hour = 0:(nrow(cent) - 1)
cent.gat = gather(cent, "cluster", value = "freq", 1:(ncol(cent) - 1))

ggplot(cent.gat, aes(x = hour, y = freq, color = cluster)) + geom_line()

station.cluster$size

routes = station[, 1:2]
routes$cluster = station.cluster$cluster

daily.routes <- df[, list(Freq = sum(count)), 
                     by = list(Start.station, End.station, Start.date)]

daily.routes <- merge(daily.routes, routes, by = c("Start.station", "End.station"))

daily.routes.cluster = daily.routes[, list(Freq = sum(Freq)), 
                                    by = list(cluster, Start.date)]

ggplot(daily.routes.cluster, aes(x = Start.date, y = Freq, color = as.factor(cluster))) + geom_line()
