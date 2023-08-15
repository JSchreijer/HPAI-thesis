#Script from Jitske Schreijer, used for the Master project "The detection of Avian Influenza from movement and behavioural data"
#starting date: 9th February 2023


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                     LOAD DATA AND START ANALYSIS                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# analysing the accelerometer data right away as it is easier to look at each bird individually, secondly, there are only 7 dead birds. 
#As we removed bird 1, 4 and 13 due to too little data or vague time of death

##### Bird 1 #####

data1 <- read.csv("200156_20220217_180000.csv")
data1 <- subset(data1, rowSums(data1[11:12] > 0) > 0)
data1 <- data1[-c(1,2),]
data1 <- sub1Bird
bird1 <- move(x=data1$Longitude, y=data1$Latitude, 
              time = as.POSIXct(paste(data1$UTC_date,data1$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
              proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
              data=data1, animal=data1$device_id, sensor = data1$datatype)
bird1DF <- as.data.frame(bird1)
sub1Bird <- bird1DF[0:147,]

acc1 <- data.frame(bird1DF$timestamps, bird1DF$acc_x, bird1DF$acc_y, bird1DF$acc_z)
acc1m <- melt(acc1, id.vars = "bird1DF.timestamps")
ggplot(acc1m, aes(x=bird1DF.timestamps, y=value)) + geom_line(aes(color=variable))


##### Bird 2 #####
#downloading data and removing zeroes
data2 <- read.csv("200160_20211123_180000.csv")
data2 <- subset(data2, rowSums(data2[11:12] > 0) > 0)

#making it into a move object, this way the move package can be used
bird2 <- move(x=data2$Longitude, y=data2$Latitude, 
              time = as.POSIXct(paste(data2$UTC_date,data2$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
              proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
              data=data2, animal=data2$device_id, sensor = data2$datatype)
#turning it back into a dataframe for using the ggplot function 

bird2DF <- as.data.frame(bird2)
#subsetting the data to look more closely at the time of death
sub2Bird <- bird2DF[585:1005,] 

#plotting accelerometer data
acc2 <- data.frame(sub2Bird$timestamps,sub2Bird$acc_x,sub2Bird$acc_y,sub2Bird$acc_z)
acc2m <- melt(acc2, id.vars = "sub2Bird.timestamps")
ggplot(acc2m, aes(x=sub2Bird.timestamps, y=value)) + geom_line(aes(color=variable))

#plotting only the x-axis of the accelerometer data
acc2 <- data.frame(sub2Bird$timestamps,sub2Bird$acc_x)
acc2m <- melt(acc2, id.vars = "sub2Bird.timestamps")
ggplot(acc2m, aes(x=sub2Bird.timestamps, y=value)) + geom_line(aes(color=variable))

#converting the date into a POSIXct so it is readable for ggplot as date
sub2Bird$UTC_date <- as.POSIXct(sub2Bird$UTC_date, format = "%d/%m/%Y")
#overimposed last days plot of the GPS tracking (Latitude)
ggplot(sub2Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "200160")

#with colour scheme black and red 
ggplot(sub2Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "200160") + scale_colour_manual(values = c("black","black","black","black",                                                                                                   "black","black","black","black",  "black","black","black", "black","black","black","black","black","black","black","black","black","black","red","red","red","red","red"))                                                                                            

#and for x-axis of the accelerometer data
ggplot(sub2Bird, aes(x=timestamps, y=acc_x, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "200160") + scale_colour_manual(values = c("black","black","black","black","black","black","black","black",  "black","black","black", "black","black","black","black","black","black","black","black","black","black","red","red","red","red","red"))                                                                                            

subsub2 <- bird2DF[801:1005,]
acc2 <- data.frame(subsub2$timestamps,subsub2$acc_x)
acc2m <- melt(acc2, id.vars = "subsub2.timestamps")
ggplot(acc2m, aes(x=subsub2.timestamps, y=value)) + geom_line(aes(color=variable))



##### Bird 3 - dead #####
#dowloading data and removing zeroes
data3 <- read.csv("200163_20220225_180000.csv")
data3 <- subset(data3, rowSums(data3[11:12] > 0) > 0)
#making it into a move object, this way the move package can be used
bird3 <- move(x=data3$Longitude, y=data3$Latitude, 
              time = as.POSIXct(paste(data3$UTC_date,data3$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
              proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
              data=data3, animal=data3$device_id, sensor = data3$datatype)
#storing it back into a datafrom to use the ggplot function 
bird3DF <- as.data.frame(bird3)
#subsetting the data to look more closely to the time of death
sub3Bird <- bird3DF[1085:1104,]

#plotting accelerometer data
acc3 <- data.frame(sub3Bird$timestamps, sub3Bird$acc_x, sub3Bird$acc_y, sub3Bird$acc_z)
acc3m <- melt(acc3, id.vars = "sub3Bird.timestamps")
ggplot(acc3m, aes(x=sub3Bird.timestamps, y=value)) + geom_line(aes(color=variable))

#plotting only the x-axis of the accelerometer data
acc3 <- data.frame(sub3Bird$timestamps, sub3Bird$acc_x)
acc3m <- melt(acc3, id.vars = "sub3Bird.timestamps")
ggplot(acc3m, aes(x=sub3Bird.timestamps, y=value)) + geom_line(aes(color=variable))

#converting the date into a POSIXct so ggplot understands that it is a date
sub3Bird$UTC_date <- as.POSIXct(sub3Bird$UTC_date, format = "%d/%m/%Y")

#overimposed last days plot of the GPS data
ggplot(sub3Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "200163")

#with colour scheme black and red
ggplot(sub3Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "200163") + scale_colour_manual(values = c("black","black","black","black","black",  "black","black", "black","black","black","black","black","black","black","black","black","black","red","red","red","red","red"))                                                                                            

#accelerometer x-axis data over time                                                                                                                                      
ggplot(sub3Bird, aes(x=timestamps, y=acc_x, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "200163") + scale_colour_manual(values = c("black","black","black","black","black",  "black","black", "black","black","black","black","black","black","black","black","black","black","black","red","red","red","red"))                                                                                            

subsub3 <- bird3DF[920:1104,]
acc3 <- data.frame(subsub3$timestamps,subsub3$acc_x)
acc3m <- melt(acc3, id.vars = "subsub3.timestamps")
ggplot(acc3m, aes(x=subsub3.timestamps, y=value)) + geom_line(aes(color=variable))

##### bird 4 - Dead ####

data4 <- read.csv("200164_20220123_180000.csv")
data4 <- subset(data4, rowSums(data4[11:12] > 0) > 0)
bird4 <- move(x=data4$Longitude, y=data4$Latitude, 
              time = as.POSIXct(paste(data4$UTC_date,data4$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
              proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
              data=data4, animal=data4$device_id, sensor = data4$datatype)
bird4DF <- as.data.frame(bird4)
sub4Bird <- bird4DF[1:70,]

acc4 <- data.frame(sub4Bird$timestamps, sub4Bird$acc_x, sub4Bird$acc_y, sub4Bird$acc_z)
acc4m <- melt(acc4, id.vars = "sub4Bird.timestamps")
ggplot(acc4m, aes(x=sub4Bird.timestamps, y=value)) + geom_line(aes(color=variable))

sub4Bird$UTC_date <- as.POSIXct(sub4Bird$UTC_date, format = "%d/%m/%Y")
sub4Bird$UTC_time <- as.POSIXct(sub4Bird$UTC_time, format = "%H:%M:%S", tz= "UTC")

ggplot(sub4Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "200163") + scale_colour_manual(values = c("black","black","black","black","black",  "black","black", "black","black","black","black","black","black","black","black","black","black","red","red","red","red","red"))                                                                                            

##### Bird 5 - Dead ####
#downloading the data and removing the zeroes
data5 <- read.csv("201912_20211224_150000.csv")
data5 <- subset(data5, rowSums(data5[11:12] > 0) > 0)

#making it into a move object, this way the move package can be used
bird5 <- move(x=data5$Longitude, y=data5$Latitude, 
              time = as.POSIXct(paste(data5$UTC_date,data5$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
              proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
              data=data5, animal=data5$device_id, sensor = data5$datatype)
#storing it back into a dataframe so the ggplot function can be used
bird5DF <- as.data.frame(bird5)
#subsetting the data so we can take a closer look at the time of death
sub5Bird <- bird5DF[1395:1497,]

#plotting accelerometer data
acc5 <- data.frame(sub5Bird$timestamps, sub5Bird$acc_x, sub5Bird$acc_y, sub5Bird$acc_z)
acc5m <- melt(acc5, id.vars = "sub5Bird.timestamps")
ggplot(acc5m, aes(x=sub5Bird.timestamps, y=value)) + geom_line(aes(color=variable))
#plotting only the x-axis
acc5 <- data.frame(sub5Bird$timestamps, sub5Bird$acc_x)
acc5m <- melt(acc5, id.vars = "sub5Bird.timestamps")
ggplot(acc5m, aes(x=sub5Bird.timestamps, y=value)) + geom_line(aes(color=variable))

#converting the date into a POSIXct so ggplot knows its a date
sub5Bird$UTC_date <- as.POSIXct(sub5Bird$UTC_date, format = "%d/%m/%Y")

#plot of the overimposed last days with GPS data
ggplot(sub5Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201912") 

#with black and red colouring
ggplot(sub5Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201912") + scale_colour_manual(values = c("black","black","black","black","black","black","black","black","black","black","black",  "black","black", "black","black","black","black","black","black","black","black","black","black","red","red","red","red","red"))                                                                                            

#for accelerometer data x-axis
ggplot(sub5Bird, aes(x=timestamps, y=acc_x, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201912") + scale_colour_manual(values = c("black","black","black","black","black","black","black","black","black","black","black",  "black","black", "black","black","black","black","black","black","black","black","red","red","red","red","red","red","red"))                                                                                            

subsub5 <- bird5DF[1290:1390,]
acc5 <- data.frame(subsub5$timestamps,subsub5$acc_x)
acc5m <- melt(acc5, id.vars = "subsub5.timestamps")
ggplot(acc5m, aes(x=subsub5.timestamps, y=value)) + geom_line(aes(color=variable))

##### Bird 6 - Alive ####

data6 <- read.csv("201913_20220506_010000.csv")
data6 <- subset(data6, rowSums(data6[11:12] > 0) > 0)
data6 <- sub6Bird
bird6 <- move(x=data6$Longitude, y=data6$Latitude, 
              time = as.POSIXct(paste(data6$UTC_date,data6$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
              proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
              data=data6, animal=data6$device_id, sensor = data6$datatype)
bird6DF <- as.data.frame(bird6)
sub6Bird <- bird6DF[0:2708,]

##### Bird 7 - Dead ####
#downloading data and removing zeroes
data7 <- read.csv("201914_20220330_150000.csv")
data7 <- subset(data7, rowSums(data7[11:12] > 0) > 0)
#changing it into a move object so the move package can be used 
bird7 <- move(x=data7$Longitude, y=data7$Latitude, 
              time = as.POSIXct(paste(data7$UTC_date,data7$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
              proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
              data=data7, animal=data7$device_id, sensor = data7$datatype)
#storing it back into a dataframe
bird7DF <- as.data.frame(bird7)
#subsetting the data so we can take a closer look to the time of death
sub7Bird <- bird7DF[505:1628,]

#plotting acclerometer data
acc7 <- data.frame(sub7Bird$timestamps, sub7Bird$acc_x, sub7Bird$acc_y, sub7Bird$acc_z)
acc7m <- melt(acc7, id.vars = "sub7Bird.timestamps")
ggplot(acc7m, aes(x=sub7Bird.timestamps, y=value)) + geom_line(aes(color=variable))

#plotting only the x-axis of the accelerometer data
acc7 <- data.frame(sub7Bird$timestamps, sub7Bird$acc_x)
acc7m <- melt(acc7, id.vars = "sub7Bird.timestamps")
ggplot(acc7m, aes(x=sub7Bird.timestamps, y=value)) + geom_line(aes(color=variable))

#converting the date and time back to datetime
sub7Bird$UTC_date <- as.POSIXct(sub7Bird$UTC_date, format = "%d/%m/%Y")

#plot of the overimposed last days with GPS data
ggplot(sub7Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201914") + scale_colour_manual(values = c("black","black", "black","black","black","black",  "black","black","black", "black","black","black","black","black","black","black","black","black","black","red","red","red","red","red"))

#for accelerometer data                                                                                                                                                                                               
ggplot(sub7Bird, aes(x=timestamps, y=acc_x, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201914") + scale_colour_manual(values = c("black","black","black","black","black","black",  "black","black","black", "black","black","black","black","black","black","black","black","black","black","red","red","red","red","red"))

subsub7 <- bird7DF[1240:1635,]
acc7 <- data.frame(subsub7$timestamps,subsub7$acc_x)
acc7m <- melt(acc7, id.vars = "subsub7.timestamps")
ggplot(acc7m, aes(x=subsub7.timestamps, y=value)) + geom_line(aes(color=variable))


acc7 <- data.frame(bird7DF$timestamps,bird7DF$acc_x, bird7DF$acc_y, bird7DF$acc_z)
acc7m <- melt(acc7, id.vars = "bird7DF.timestamps")
ggplot(acc7m, aes(x=bird7DF.timestamps, y=value)) + geom_line(aes(color=variable))

##### Bird 8 - Dead ####
#downloading data and removing zeroes
data8 <- read.csv("201915_20211215_180000.csv")
data8 <- subset(data8, rowSums(data8[11:12] > 0) > 0)
#converting it into a move object so we can use the move package 
bird8 <- move(x=data8$Longitude, y=data8$Latitude, 
              time = as.POSIXct(paste(data8$UTC_date,data8$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
              proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
              data=data8, animal=data8$device_id, sensor = data8$datatype)
#changing it back into a dataframe so the ggplot function can be used
bird8DF <- as.data.frame(bird8)
#subsetting the data so we can take a closer look to the time of death
sub8Bird <- bird8DF[2130:2170,]

#plotting the accelerometer data
acc8 <- data.frame(sub8Bird$timestamps, sub8Bird$acc_x, sub8Bird$acc_y,sub8Bird$acc_z)
acc8m <- melt(acc8, id.vars = "sub8Bird.timestamps")
ggplot(acc8m, aes(x=sub8Bird.timestamps, y=value)) + geom_line(aes(color=variable))

#plotting the x-axis of the accelerometer data
acc8 <- data.frame(sub8Bird$timestamps, sub8Bird$acc_x)
acc8m <- melt(acc8, id.vars = "sub8Bird.timestamps")
ggplot(acc8m, aes(x=sub8Bird.timestamps, y=value)) + geom_line(aes(color=variable))
#converting the date to POSIXct so ggplot understands that it is a date
sub8Bird$UTC_date <- as.POSIXct(sub8Bird$UTC_date, format = "%d/%m/%Y")

#overimposed last days plot with GPS data
ggplot(sub8Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201915")

#with colour scheme black and red
ggplot(sub8Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201915") + scale_colour_manual(values = c("black","black","black","black","black","black",  "black","black","black", "black","black","black","black","black","black","black","black","black","black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black","red","red","red","red","red"))                                                                                         

#for accelerometer data
ggplot(sub8Bird, aes(x=timestamps, y=acc_x, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201915") + scale_colour_manual(values = c("black","black","black","black","black","black",  "black","black","black", "black","black","black","black","black","black","black","black","black","black", "black", "black", "black", "black", "black", "black", "black", "black", "black","red", "red","red","red","red","red"))                                                                                         

subsub8 <- bird8DF[1800:2160,]
acc8 <- data.frame(subsub8$timestamps, subsub8$acc_x)
acc8m <- melt(acc8, id.vars = "subsub8.timestamps")
ggplot(acc8m, aes(x=subsub8.timestamps, y=value)) + geom_line(aes(color=variable))

##### Bird 9 -Alive #####
data9 <- read.csv("201917_20220427_230000.csv")
data9 <- subset(data9, rowSums(data9[11:12] > 0) > 0)
bird9 <- sub9Bird
bird9 <- move(x=data9$Longitude, y=data9$Latitude, 
              time = as.POSIXct(paste(data9$UTC_date,data9$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
              proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
              data=data9, animal=data9$device_id, sensor = data9$datatype)
bird9DF <- as.data.frame(bird9)
sub9Bird <- bird9DF[0:1071,]

##### Bird 10 - Alive ####

data10 <- read.csv("201918_20220418_220000.csv")
data10 <- subset(data10, rowSums(data10[11:12] > 0) > 0)
bird10 <- sub10Bird
bird10 <- move(x=data10$Longitude, y=data10$Latitude, 
               time = as.POSIXct(paste(data10$UTC_date,data10$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data10, animal=data10$device_id, sensor = data10$datatype)
bird10DF <- as.data.frame(bird10)
sub10Bird <- bird10DF[0:855,]

##### Bird 11 - Alive ####

data11 <- read.csv("201923_20220427_230000.csv")
data11 <- subset(data11, rowSums(data11[11:12] > 0) > 0)
data11 <- sub11Bird
bird11 <- move(x=data11$Longitude, y=data11$Latitude, 
               time = as.POSIXct(paste(data11$UTC_date,data11$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data11, animal=data11$device_id, sensor = data11$datatype)
bird11DF <- as.data.frame(bird11)
sub11Bird <- bird11DF[0:1026,]

##### Bird 12 - Alive #####

data12 <- read.csv("201925_20220428_220000.csv")
data12 <- subset(data12, rowSums(data12[11:12] > 0) > 0)
data12 <- sub12Bird
bird12 <- move(x=data12$Longitude, y=data12$Latitude, 
               time = as.POSIXct(paste(data12$UTC_date,data12$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data12, animal=data12$device_id, sensor = data12$datatype)
bird12DF <- as.data.frame(bird12)
sub12Bird <- bird12DF[0:363,]

##### Bird 13 - Dead #####
data13 <- read.csv("201926_20211123_200000.csv")
data13 <- subset(data13, rowSums(data13[11:12] > 0) > 0)
data13 <- sub13Bird
bird13 <- move(x=data13$Longitude, y=data13$Latitude, 
               time = as.POSIXct(paste(data13$UTC_date,data13$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data13, animal=data13$device_id, sensor = data13$datatype)
bird13DF <- as.data.frame(bird13)
sub13Bird <- bird13DF[560:582,]

acc13 <- data.frame(sub13Bird$timestamps, sub13Bird$acc_x, sub13Bird$acc_y, sub13Bird$acc_z)
acc13m <- melt(acc13, id.vars = "sub13Bird.timestamps")
ggplot(acc13m, aes(x=sub13Bird.timestamps, y=value)) + geom_line(aes(color=variable))

##### Bird 14 - Alive #####

data14 <- read.csv("201927_20220429_000000.csv")
data14 <- subset(data14, rowSums(data14[11:12] > 0) > 0)
data14 <- sub14Bird
bird14 <- move(x=data14$Longitude, y=data14$Latitude, 
               time = as.POSIXct(paste(data14$UTC_date,data14$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data14, animal=data14$device_id, sensor = data14$datatype)
bird14DF <- as.data.frame(bird14)
sub14Bird <- bird14DF[0:1189,]

##### BIrd 15 - Dead #####
#downloading the data and removing zeroes
data15 <- read.csv("201928_20211116_090000.csv")
data15 <- subset(data15, rowSums(data15[11:12] > 0) > 0)
#changing the data into a move object so the move package can be used
bird15 <- move(x=data15$Longitude, y=data15$Latitude, 
               time = as.POSIXct(paste(data15$UTC_date,data15$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data15, animal=data15$device_id, sensor = data15$datatype)
#storing it back into a dataframe to use the ggplot function later
bird15DF <- as.data.frame(bird15)
#subsetting the data to take a closer look at the time of death
sub15Bird <- bird15DF[950:1008,]

#plotting accelerometer data
acc15 <- data.frame(sub15Bird$timestamps, sub15Bird$acc_x, sub15Bird$acc_y, sub15Bird$acc_z)
acc15m <- melt(acc15, id.vars = "sub15Bird.timestamps")
ggplot(acc15m, aes(x=sub15Bird.timestamps, y=value)) + geom_line(aes(color=variable))
#plotting the x-axis of the accelerometer data
acc15 <- data.frame(sub15Bird$timestamps, sub15Bird$acc_x)
acc15m <- melt(acc15, id.vars = "sub15Bird.timestamps")
ggplot(acc15m, aes(x=sub15Bird.timestamps, y=value)) + geom_line(aes(color=variable))
#converting the date into a POSIXct so ggplot understands that its a date
sub15Bird$UTC_date <- as.POSIXct(sub15Bird$UTC_date, format = "%d/%m/%Y")


#overimposed last days plot with GPS data
ggplot(sub15Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201928")

#with colour scheme black and red
ggplot(sub15Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201928") + scale_colour_manual(values = c("black", "black","black","black","black","black","black","black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black","red","red","red","red","red"))                                                                                         

#for accelerometer data
ggplot(sub15Bird, aes(x=timestamps, y=acc_x, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201928") + scale_colour_manual(values = c("black", "black","black","black","black","black","black","black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black","red","red","red","red","red"))                                                                                         

subsub15 <- bird15DF[750:995,]
acc15 <- data.frame(subsub15$timestamps, subsub15$acc_x)
acc15m <- melt(acc15, id.vars = "subsub15.timestamps")
ggplot(acc15m, aes(x=subsub15.timestamps, y=value)) + geom_line(aes(color=variable))


#### Bird 16 - Dead #####
#donwloading data and removing zeroes
data16 <- read.csv("201929_20211204_110000.csv")
data16 <- subset(data16, rowSums(data16[11:12] > 0) > 0)
#converting the data into a move object so the move package can be used
bird16 <- move(x=data16$Longitude, y=data16$Latitude, 
               time = as.POSIXct(paste(data16$UTC_date,data16$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data16, animal=data16$device_id, sensor = data16$datatype)
#storing it as a dataframe so we can use the ggplot function later
bird16DF <- as.data.frame(bird16)
#subsetting the data to take a closer look at the time of death
sub16Bird <- bird16DF[995:1028,]
#plotting the accelerometer data
acc16 <- data.frame(sub16Bird$timestamps,sub16Bird$acc_x, sub16Bird$acc_y,sub16Bird$acc_z)
acc16m <- melt(acc16, id.vars = "sub16Bird.timestamps")
ggplot(acc16m, aes(x=sub16Bird.timestamps, y=value)) + geom_line(aes(color=variable))
#plotting the x-axis of the accelerometer data
acc16 <- data.frame(sub16Bird$timestamps,sub16Bird$acc_x)
acc16m <- melt(acc16, id.vars = "sub16Bird.timestamps")
ggplot(acc16m, aes(x=sub16Bird.timestamps, y=value)) + geom_line(aes(color=variable))
#converting the date into a POSIXct so ggplot understands that it is a date
sub16Bird$UTC_date <- as.POSIXct(sub16Bird$UTC_date, format = "%d/%m/%Y")

#overimposed last days plot with GPS data
ggplot(sub16Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201929")

#with colour scheme black and red
ggplot(sub16Bird, aes(x=UTC_time, y=Latitude, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201929") + scale_colour_manual(values = c("black", "black","black","black","black","black","black","black","black","black","black","black","black","black","black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black","red","red","red","red","red"))                                                                                         
#plot of the accelerometer data with colour scheme black and red
ggplot(sub16Bird, aes(x=timestamps, y=acc_x, group=UTC_date)) + geom_point(aes(colour = factor(UTC_date))) + 
  geom_line(aes(colour = factor(UTC_date))) +
  theme_bw()+labs(colour="Date") + labs(title = "201929") + scale_colour_manual(values = c("black", "black","black","black","black","black","black","black","black","black","black","black","black","black","black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "red","red","red","red","red","red"))                                                                                         

subsub16 <- bird16DF[860:1026,]
acc16 <- data.frame(subsub16$timestamps, subsub16$acc_x)
acc16m <- melt(acc16, id.vars = "subsub16.timestamps")
ggplot(acc16m, aes(x=subsub16.timestamps, y=value)) + geom_line(aes(color=variable))

##### Bird 17 - 21 - Alive #####

data17 <- read.csv("201930_20220506_030000.csv")
data17 <- subset(data17, rowSums(data17[11:12] > 0) > 0)
data17 <- sub17Bird
bird17 <- move(x=data17$Longitude, y=data17$Latitude, 
               time = as.POSIXct(paste(data17$UTC_date,data17$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data17, animal=data17$device_id, sensor = data17$datatype)
bird17DF <- as.data.frame(bird17)
sub17Bird <- bird17DF[0:637,]

data18 <- read.csv("201931_20220428_220000.csv")
data18 <- subset(data18, rowSums(data18[11:12] > 0) > 0)
data18 <- sub18Bird
bird18 <- move(x=data18$Longitude, y=data18$Latitude, 
               time = as.POSIXct(paste(data18$UTC_date,data18$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data18, animal=data18$device_id, sensor = data18$datatype)
bird18DF <- as.data.frame(bird18)
sub18Bird <- bird18DF[0:1528,]

data19 <- read.csv("201932_20220429_200000.csv")
data19 <- subset(data19, rowSums(data19[11:12] > 0) > 0)
data19 <- sub19Bird
bird19 <- move(x=data19$Longitude, y=data19$Latitude, 
               time = as.POSIXct(paste(data19$UTC_date,data19$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data19, animal=data19$device_id, sensor = data19$datatype)
bird19DF <- as.data.frame(bird19)
sub19Bird <- bird19DF[0:1293,]

data20 <- read.csv("201934_20220429_210000.csv")
data20 <- subset(data20, rowSums(data20[11:12] > 0) > 0)
data20 <- sub20Bird
bird20 <- move(x=data20$Longitude, y=data20$Latitude, 
               time = as.POSIXct(paste(data20$UTC_date,data20$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data20, animal=data20$device_id, sensor = data20$datatype)
bird20DF <- as.data.frame(bird20)
sub20Bird <- bird20DF[0:1840,]

data21 <- read.csv("201935_20220429_200000.csv")
data21 <- data21[-7661,]
data21 <- subset(data21, rowSums(data21[11:12] > 0) > 0)
data21 <- sub21Bird
bird21 <- move(x=data21$Longitude, y=data21$Latitude, 
               time = as.POSIXct(paste(data21$UTC_date,data21$UTC_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data21, animal=data21$device_id, sensor = data21$datatype)
bird21DF <- as.data.frame(bird21)
sub21Bird <- bird21DF[0:988,]

data22 <- read.csv("201974_20230131_122600.csv")
data22 <- data22[,-23]
data22 <- subset(data22, rowSums(data22[11:12] > 0) > 0)
data22 <- sub22Bird
bird22 <- move(x=data22$Longitude, y=data22$Latitude, 
               time = as.POSIXct(paste(data22$UTC_date,data22$UTC_time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
               proj = CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"),
               data=data22, animal=data22$device_id, sensor = data22$datatype)
bird22DF <- as.data.frame(bird22)
sub22Bird <- bird22DF[0:1105,]


