sub7Bird <- bird7DF[150:1614,]



Daily_summary7<- sub7Bird %>%
  group_by(UTC_date, device_id) %>%
  dplyr::summarise(Var_acc_x=var(acc_x), ar1_acc_x=ar1(acc_x, method='yw')[2], 
            skewness_acc_x=skewness(acc_x))

Daily_summary9<- sub9Bird %>%
  group_by(UTC_date, device_id) %>%
  dplyr::summarise(Var_acc_x=var(acc_x), ar1_acc_x=ar1(acc_x, method='yw')[2], 
                   skewness_acc_x=skewness(acc_x)) 

Daily_summary10<- sub10Bird %>%
  group_by(UTC_date, device_id) %>%
  dplyr::summarise(Var_acc_x=var(acc_x), ar1_acc_x=ar1(acc_x, method='yw')[2], 
                   skewness_acc_x=skewness(acc_x)) 

Daily_summary11<- sub11Bird %>%
  group_by(UTC_date, device_id) %>%
  dplyr::summarise(Var_acc_x=var(acc_x), ar1_acc_x=ar1(acc_x, method='yw')[2], 
                   skewness_acc_x=skewness(acc_x)) 

Daily_summary12<- sub12Bird %>%
  group_by(UTC_date, device_id) %>%
  dplyr::summarise(Var_acc_x=var(acc_x), ar1_acc_x=ar1(acc_x, method='yw')[2], 
                   skewness_acc_x=skewness(acc_x)) 

daily <- rbind(Daily_summary7, Daily_summary9, Daily_summary10, Daily_summary11, Daily_summary12)


daily$UTC_date <- as.Date(daily$UTC_date, format = "%d/%m/%Y")
Daily_summary7$UTC_date <- as.Date(Daily_summary7$UTC_date, format = "%d/%m/%Y")
daily$device_id <- as.character(daily$device_id)
Daily_summary7$device_id <- as.character(Daily_summary7$device_id)

p <- ggplot(daily, aes(x=UTC_date, y=skewness_acc_x, col = device_id)) +
  geom_line() +  facet_grid(rows = vars(device_id)) + xlab("") + scale_x_date(date_labels = "%d/%m/%Y")  + theme(axis.text.x=element_text(angle=60, hjust=1)) 
p + ylab("Skewness") +  scale_color_manual(values=c("red",'black', "black", "black", "black")) +
  geom_rect(aes(xmin = as.Date("2021-10-17"),xmax = as.Date("2021-11-13"),ymin = -1.15, ymax = Inf),
            fill="green", 
            alpha = .005)+
  geom_rect(aes(xmin = as.Date("2021-11-13"),xmax = as.Date("2021-11-16"),ymin = -1.1, ymax = Inf),
            fill="orange", 
            alpha = .005) + theme_bw() 
#sick birds
p <- ggplot(Daily_summary7, aes(x=UTC_date, y=skewness_acc_x, col = device_id)) +
  geom_line() +  facet_grid(rows = vars(device_id)) + xlab("") + scale_x_date(date_labels = "%d/%m/%Y")  + theme(axis.text.x=element_text(angle=60, hjust=1)) 
p + ylab("Skewness") +  scale_color_manual(values=c( "red")) +
  geom_rect(aes(xmin = as.Date("2021-10-17"),xmax = as.Date("2021-11-13"),ymin = -1.15, ymax = Inf),
            fill="green", 
            alpha = .005)+
  geom_rect(aes(xmin = as.Date("2021-11-13"),xmax = as.Date("2021-11-16"),ymin = -1.1, ymax = Inf),
            fill="red", 
            alpha = .005) + theme_bw() 




p <- ggplot(daily, aes(x=UTC_date, y=Var_acc_x, col = device_id)) +
  geom_line() +  facet_grid(rows = vars(device_id)) + xlab("") + scale_x_date(date_labels = "%d/%m/%Y")  + theme(axis.text.x=element_text(angle=60, hjust=1)) 
p + ylab("Variance") +  scale_color_manual(values=c("red",'black', "black", "black", "black")) +
  geom_rect(aes(xmin = as.Date("2021-10-17"),xmax = as.Date("2021-11-13"),ymin = -1.15, ymax = Inf),
            fill="green", 
            alpha = .005)+
  geom_rect(aes(xmin = as.Date("2021-11-13"),xmax = as.Date("2021-11-16"),ymin = -1.1, ymax = Inf),
            fill= letters[1:5],
            alpha = .005) + theme_bw() 


###boxplot showing sickness, pre-sickness and healhty birds skewness
results_larry <- results[1:21,]


ggplot(finished,aes(event,log(home_range), fill = event)) + 
  geom_boxplot(fill = c("green", "green", "red"), alpha=0.2) + theme_bw() + xlab("Status") +  scale_color_manual(values=c("black",'red', "red"))+
  ylab("Home range")

ggplot(resultsone,aes(event,variance, fill = event)) + 
  geom_boxplot(fill = c("green", "green", "red"), alpha=0.2) + theme_bw() + xlab("Status")


ggplot(resultsone,aes(event,autocorrelation, fill = event)) + 
  geom_boxplot(fill = c("green", "green", "red"), alpha=0.2) + theme_bw() + xlab("Status")

ggplot(resultsone,aes(event,skewness, fill = event)) + 
  geom_boxplot(fill = c("green", "green", "red"), alpha=0.2) + theme_bw() + xlab("Status") + ylab("Home range")


results$home_range <- resultstwo$home_range
