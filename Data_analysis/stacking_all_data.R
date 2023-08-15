


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           stacking all datasets                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stackdead <- moveStack(bird2, bird3, bird5, bird7, bird8, bird15,  bird16)
stackalive <- moveStack(bird6, bird9, bird10, bird11, bird12,bird14,bird17
                        , bird18, bird19, bird20, bird21, bird22)
save(stackdead, file ="stackedbirds.RData")

#creating a dataframe from said stack
stackdeadDF <- as.data.frame(stackdead)
stackaliveDF <- as.data.frame(stackalive)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           adding time of death and exploration                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#downloading time of death for each bird, this data table was made manually based on accelerator data that was made earlier

death_dates<-read_excel('Time_of_Death_Birds.xlsx', skip=1)
str(death_dates)

# adding time of death to all birds

death_list<-unstack(stackdead)

stackdead@data<- merge(cbind(stackdead@data, device_id=substring(as.character(stackdead@trackId), 2)), death_dates) 

# time to death

stackdead@data$UTC_datetime<-as.POSIXct(stackdead@data$UTC_datetime, tz='UTC', format='%d/%m/%Y %H:%M')
stackalive@data$UTC_datetime<-as.POSIXct(stackalive@data$UTC_datetime, tz='UTC', format='%d/%m/%Y %H:%M')


stackdead@data$days_to_death <- as.numeric(difftime(stackdead@data$`time of death`, stackdead@data$UTC_datetime, units='days'))

stackdead@data$days_to_death_round<--1*round(stackdead@data$days_to_death)


# ok, now let's look at some animal and explore what do we have there..

Cur_bird='200163'

Cur_data<-stackdead@data %>% filter(device_id==Cur_bird & days_to_death > 0)
plot(acc_x~UTC_datetime, data=Cur_data, type='l')



# variance per day for now..
Daily_summary<-Cur_data %>%
  group_by(days_to_death_round) %>%
  summarise(Var_acc_x=var(acc_x), ar1_acc_x=ar1(acc_x, method='yw')[2], 
            skewness_acc_x=skewness(acc_x))

par(mfrow=c(1,3))
plot(Daily_summary$Var_acc_x~Daily_summary$days_to_death_round, type='l', main='variance')
plot(Daily_summary$skewness_acc_x~Daily_summary$days_to_death_round, type='l', main='skewness')
plot(Daily_summary$ar1_acc_x~Daily_summary$days_to_death_round, type='l', main='autocorrelation (ar1)')

plot(rev((Cur_data$acc_x), type='l'))

#remaking stackdeadDF so it contains all new columns
stackdeadDF <- as.data.frame(stackdead)
stackaliveDF <- as.data.frame(stackalive)