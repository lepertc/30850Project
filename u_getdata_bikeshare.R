library(data.table)

data = NULL

for(i in 2010:2011){
	file = paste0('https://s3.amazonaws.com/capitalbikeshare-data/',i,'-capitalbikeshare-tripdata.zip')
	download.file(file,destfile='bikedata.zip')
	unzip('bikedata.zip')
	data = rbind(data,read.csv(paste0(i,'-capitalbikeshare-tripdata.csv')))
}

for(i in 2012:2017){
  file = paste0('https://s3.amazonaws.com/capitalbikeshare-data/',i,'-capitalbikeshare-tripdata.zip')
  download.file(file,destfile='bikedata.zip')
  unzip('bikedata.zip')
  data = rbind(data,read.csv(paste0(i,'Q1-capitalbikeshare-tripdata.csv')))
  data = rbind(data,read.csv(paste0(i,'Q2-capitalbikeshare-tripdata.csv')))
  data = rbind(data,read.csv(paste0(i,'Q3-capitalbikeshare-tripdata.csv')))
  data = rbind(data,read.csv(paste0(i,'Q4-capitalbikeshare-tripdata.csv')))
}

df = as.data.table(data)
rm(data)

df$Start.time = substr(as.character(df$Start.date), 12, 19)
df$Start.date = as.Date(substr(as.character(df$Start.date), 1, 10))
df$Start.hour = as.numeric(substr(df$Start.time, 1,2))
df$End.time = substr(as.character(df$End.date), 12, 19)
df$End.date = as.Date(substr(as.character(df$End.date), 1, 10))

names(df)[1] = "Duration.seconds"

