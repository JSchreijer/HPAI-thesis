library(ggplot2)
library(reshape2)
library(move)

# read and plot 1 file:

filename<-'NIOOBG_192739_20210227_000000.csv'
data <- read_csv(filename) 


View(data_gps)

read_and_plot<-function(filename) {
  
  data <- read_csv(filename) 
  if (is.character(data$UTC_datetime[1])) data$UTC_datetime <-as.POSIXct(data$UTC_datetime, format='%m/%d/%Y %H:%M', tz='UTC')
  data_gps<-data %>% filter(!is.na(Latitude)) %>% filter(Latitude!=0)
  
#   data_move <- move(x=data_gps$Longitude, y=data_gps$Latitude, 
 #                   time=data_gps$UTC_datetime, 
  #                  proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
   #                 data=data_gps, animal=data_gps$device_id)

# Convert move object to a dataframe
data_df <- as.data.frame(data_move)

# Subset the data for a specific time range
subset<- data %>% filter(as.Date(data$UTC_datetime) >= (as.Date(max(data$UTC_datetime))-10)) %>%  filter(Latitude!=0)


pdf(file=paste0('bird',strsplit(filename, '\\.')[[1]][1],'.pdf'), width=21, height=7)
par(mfrow=c(1,3))
#plot map 
plot(data_gps$Latitude ~ data_gps$Longitude)
plot(wrld_simpl, add=T, col='grey')

lines(data_gps$Latitude ~ data_gps$Longitude, col='red', lwd=2)

points(data_gps$Latitude ~ data_gps$Longitude, col='red', pch='+')

# last point blue
points(rev(data_gps$Latitude)[1] ~ rev(data_gps$Longitude)[1], col='blue', pch='+', cex=2)


# Plot accelerometer data using ggplot2
plot(data_gps$acc_x  ~ data_gps$UTC_datetime, type='l', col='red', ylim=c(-1600, 1600), lwd=2)

lines(data_gps$acc_y  ~ data_gps$UTC_datetime, type='l', col='blue', lwd=2)

lines(data_gps$acc_z  ~ data_gps$UTC_datetime, type='l', col='green', lwd=2)

subset<- data %>% filter(as.Date(data$UTC_datetime) >= (as.Date(max(data$UTC_datetime))-10)) %>%  filter(Latitude!=0)

plot(subset$acc_x  ~ subset$UTC_datetime, type='l', col='red', ylim=c(-1600, 1600), lwd=2)

lines(subset$acc_y  ~ subset$UTC_datetime, type='l', col='blue', lwd=2)

lines(subset$acc_z  ~ subset$UTC_datetime, type='l', col='green', lwd=2)

dev.off()
}
Files<-list.files(pattern='*\\.csv$')

# now loop over all files...
for (i in 1:length(Files)) {
  filename<-Files[i]
  read_and_plot(filename)

}




