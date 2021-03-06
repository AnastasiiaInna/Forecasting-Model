# ------- STL ETS Model ------ #
# Forecasting set-up
if (!("horizon" %in% colnames(input)) & "test.length" %in% colnames(input)) {horizon <- input$test.length}

# Forecasting Function
stl.single.id <- function(data, method){
  
  method.name <- method
  
  # Train and test split
  data.length <- nrow(data)
  train.length <- data.length - horizon
  train <- data[1:train.length, ]
  test <- data[(train.length+1):data.length, ]
  
  # Missing data: replace na with average
  train$Volume[is.na(train$Volume)] <- mean(train$Volume, na.rm = TRUE)
  
  # Missing dates
  x <- zoo(train$Volume, (train$Date))    
  empty <- zoo(order.by = seq.Date(head(index(x), 1), tail(index(x), 1), by = "month"))  
  # x <- na.locf(merge(x, empty)) # replace with the volumes from previous month  
  # Replace with zeros
  x <- (merge(x, empty))
  x[which(is.na(x))] <- 0.0
  start.date <- min(train$Date)
  train.ts <- ts(x,  frequency = seasonality, start = c(year(start.date), month(start.date)))
  
  # Build forecasting models
  train.stl <- stl(train.ts, s.window = "periodic")
  
  m <- frequency(train.stl$time.series)
  n <- nrow(train.stl$time.series)
  h <- horizon
  
  lastseas <- rep(train.stl$time.series[n - (m:1) + 1, "seasonal"], trunc(1 + (h - 1)/m))[1:h]
  start <-(as.Date(yearmon(index(as.ts(seasadj(train.stl))))))[1] + 1
  len<-length(as.Date(yearmon(index(as.ts(seasadj(train.stl))))))
  
  z<-as.zoo(seasadj(train.stl))
  sa <- as.ts(z)
  
  if (method != "STL_REG") {
    
    if(method == "STL_ETS") {
      fcast <- forecast(ets(sa, model = "ZZN",damped = NULL), h = horizon, level = c(80, 95) ,ic = 'bic', opt.crit = 'mae')
    }
    else if(method == "STL_ARIMA") { 
      fcast <- forecast(auto.arima(sa), h = horizon, level = c(80, 95))
    }
    else if(method == "STL_snaive") {
      fcast <- rwf(sa, h = horizon, drift = TRUE)
    }
    
    fcast$mean <- fcast$mean + lastseas
    fcast$upper <- fcast$upper + lastseas
    fcast$lower <- fcast$lower + lastseas
    fcast$x <- ts(rowSums(train.stl$time.series))
    
    output <- data.frame(Date = test$Date, fcast)
    colnames(output)[-1] <- paste(c("forecast", "lo80", "hi80", "lo95", "hi95"), method.name, sep = ".") 
  }
  
  else if (method == "STL_REG") {
    sad <- data.frame(Date = as.Date(yearmon(index(sa))),  Volume = coredata(sa))
    reg <- subset(regressor,(regressor$Date >= as.POSIXct(min(yearmon(index(sa)))) & regressor$Date <= as.POSIXct(max(yearmon(index(sa))))))
    fcast <- subset(regressor,(regressor$Date > as.POSIXct(max(yearmon(index(sa)))) & regressor$Date <= as.POSIXct(max(yearmon(index(sa)+1)))))
    fit.lm <- step(lm(Volume ~ Trend,  data = join(sad, reg, by = c("Date"), type = "right")), direction = "backward")
    
    fcast$forecast <- predict.lm(fit.lm, newdata = fcast) + lastseas
    fcast$lo80 <- predict.lm(fit.lm, newdata = fcast, interval = "confidence", level = .8)[, 'lwr'] + lastseas
    fcast$hi80 <- predict.lm(fit.lm, newdata = fcast, interval = "confidence", level = .8)[, 'upr'] + lastseas
    fcast$lo95 <- predict.lm(fit.lm, newdata = fcast, interval = "confidence", level = .95)[, 'lwr'] + lastseas
    fcast$hi95 <- predict.lm(fit.lm, newdata = fcast, interval = "confidence", level = .95)[, 'upr'] + lastseas
    fcast$Trend<-NULL
    colnames(fcast)[-1] <- paste(c("forecast", "lo80", "hi80", "lo95", "hi95"), method.name, sep = ".") 
    output <- fcast
  }
  
  forecast.result <- output
  forecast.result$Method <- method
  return(output)
}



