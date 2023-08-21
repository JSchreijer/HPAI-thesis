#For loop of the skewness, variance, autocorrelation and ctmm analysis
tableA <-read_excel('movementstack@tableA.xlsx')

level <- 0.95 # we want to display 95% confidence intervals
xlim <- c(0, 2 %#% "month") # to create a window of 2 months

results <- data.frame(device_id = character(),
                      skewness = numeric(),
                      variance = numeric(),
                      autocorrelation = numeric(),
                      home_range = numeric(),
                      stringsAsFactors = FALSE)


for (i in 1:length(tableA$look_at_id)){
  name <- tableA$look_at_id[i]
  move_obj <- stackall[[name]]
move_obj$UTC_date <- as.POSIXct(move_obj$UTC_date)
    
  start_date <- tableA$start_date[i]
  end_date <- tableA$end_date[i]
  
 
  # Filter move_obj data based on date condition
  move_obj_filtered <- move_obj[which(move_obj@data$UTC_date >= start_date & move_obj@data$UTC_date <= end_date)]
  
  print(dim(move_obj_filtered))
  
  if (nrow(move_obj_filtered) > 0) {
    # Calculate skewness, variance, and autocorrelation per day
    Daily_summary <- move_obj_filtered@data %>%
      group_by(UTC_date) %>%
      dplyr::summarise(Var_acc_x = var(acc_x), ar1_acc_x = ar1(acc_x, method = 'yw')[2], 
                skewness_acc_x = skewness(acc_x), indicator = sum(acc_x<(-600))/n(),
               interq = diff(quantile(acc_x, c(0.25, 0.75))))
     
    
mcp(move_obj_filtered, percent = 80, unin = "m", unout = "m2")
  
  # Convert move_obj to telemetry format
telemetry_stack <- as.telemetry(move_obj_filtered)

  GUESS1 <- ctmm.guess(telemetry_stack, interactive = FALSE)
  FIT1_ML <- ctmm.select(telemetry_stack, GUESS1, method = "ML")
  FIT1_pHREML <- ctmm.select(telemetry_stack, GUESS1, method = "pHREML")
  UD1_pHREML <- akde(telemetry_stack, FIT1_pHREML)
  
  results[i, "device_id"] <- move_obj@idData$device_id
  results[i, "skewness"] <- mean(Daily_summary$skewness_acc_x, na.rm = TRUE)
  results[i, "variance"] <- mean(Daily_summary$Var_acc_x, na.rm = TRUE)
  results[i, "autocorrelation"] <- mean(Daily_summary$ar1_acc_x, na.rm = TRUE)
  results[i, "indicator"] <- mean(indicator, na.rm = TRUE)
  results[i, "interq"] <- mean(interq, na.rm = TRUE)
  results[i, "home_range"] <- summary(UD1_pHREML)$CI  
  }
}

# Print the results
print(results)



# THis part of the code was first behind the telemetry stack line but taken out to check for errors in the for loop 
#SVF <- variogram(telemetry_stack)
#par(mfrow = c(1, 2))
#plot(SVF, fraction = 1, level = level)
#abline(v = 1, col = "red", lty = 2) # adding a line at 1 month 
#plot(SVF, xlim = xlim, level = level)
#abline(v = 1, col = "red", lty = 2)
