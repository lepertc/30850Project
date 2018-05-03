df$count <- 1

start.station <- df[, list(Freq = sum(count), length.mean = mean(Duration.seconds)),
            by = list(Start.station, Start.station.number)]

end.station <- df[, list(Freq = sum(count), length.mean = mean(Duration.seconds)),
                    by = list(End.station, End.station.number)]

stations <- merge(start.station, end.station, by.x = c("Start.station", "Start.station.number"), 
                  by.y = c("End.station", "End.station.number"), all = T)
stations$length.mean.x = NULL
stations$length.mean.y = NULL
names(stations) <- c("name", "number", "start.freq", "end.freq")
write.csv(stations, "stations.csv")
