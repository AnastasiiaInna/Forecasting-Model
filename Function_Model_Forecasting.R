forecast.result <- function(data, method){

  # Missing data: replace na with average
  data$Volume[is.na(data$Volume)] <- mean(data$Volume, na.rm = TRUE)
  
  # Missing dates
  x <- zoo(data$Volume, as.Date(data$Date))    
  empty <- zoo(order.by = seq.Date(head(index(x), 1), tail(index(x), 1), by = "month"))  
  # x <- na.locf(merge(x, empty)) # replace with the volumes from previous month  
  
  # Replace with zeros
  x <- (merge(x, empty))
  x[which(is.na(x))] <- 0.0 # replace with zeros
  start.date <- min(data$Date)
  data.ts <- ts(x,  frequency = seasonality, start = c(year(start.date), month(start.date)))
  
  # Build forecasting models
  data.stl <- stl(data.ts, s.window = "periodic")
  
  m <- frequency(data.stl$time.series)
  n <- nrow(data.stl$time.series)
  h <- horizon
  
  lastseas <- rep(data.stl$time.series[n - (m:1) + 1, "seasonal"], trunc(1 + (h - 1)/m))[1:h]
  start <-(as.Date(yearmon(index(as.ts(seasadj(data.stl))))))[1] + 1
    len<-length(as.Date(yearmon(index(as.ts(seasadj(data.stl))))))
  
  z <- as.zoo(seasadj(data.stl))
  sa <- as.ts(z)
  
  if (method != "STL_REG") {
    
    if(method == "STL_ETS") {
      fcast <- forecast(ets(sa,model = "ZZN", damped = NULL), h = forecast.length, level = c(80, 95) ,ic = 'bic', opt.crit = 'mae')
    }
    else if(method == "STL_ARIMA") { 
      fcast <- forecast(auto.arima(sa), h = forecast.length, level = c(80, 95))
    }
    else if(method == "STL_snaive") {
      fcast <- rwf(sa, h = forecast.length, drift = TRUE)
    }
    
    fcast$mean <- fcast$mean + lastseas
    fcast$upper <- fcast$upper + lastseas
    fcast$lower <- fcast$lower + lastseas
    
    output <- data.frame(Date = as.Date(time(fcast$mean)), cbind(fcast$mean, fcast$upper, fcast$lower))
    colnames(output)[-1] <- paste(c("forecast", "hi80", "hi95", "lo80", "lo95"), sep = ".") 
  }
  
  else if (method == "STL_REG") {
    sad <- data.frame(Date = as.Date(yearmon(index(sa))),  Volume = coredata(sa))
    reg <- subset(regressor,(regressor$Date >= as.POSIXct(min(yearmon(index(sa)))) & regressor$Date <= as.POSIXct(max(yearmon(index(sa))))))
    fcast <- subset(regressor,(regressor$Date > as.POSIXct(max(yearmon(index(sa)))) & regressor$Date <= as.POSIXct(max(yearmon(index(sa)+2)))))
    fit.lm <- step(lm(Volume ~ Trend, data = join(sad, reg, by = c("Date"), type = "right")), direction = "backward")
    
    fcast$forecast <- predict.lm(fit.lm, newdata = fcast) + lastseas
    fcast$lo80 <- predict.lm(fit.lm, newdata = fcast, interval = "confidence", level = .8)[, 'lwr'] + lastseas
    fcast$hi80 <- predict.lm(fit.lm, newdata = fcast, interval = "confidence", level = .8)[, 'upr'] + lastseas
    fcast$lo95 <- predict.lm(fit.lm, newdata = fcast, interval = "confidence", level = .95)[, 'lwr'] + lastseas
    fcast$hi95 <- predict.lm(fit.lm, newdata = fcast, interval = "confidence", level = .95)[, 'upr'] + lastseas
    fcast$Trend <- NULL
    colnames(fcast)[-1] <- paste(c("forecast", "lo80", "hi80", "lo95", "hi95"), sep = ".") 
    output <- fcast
  }
  
  forecast.result <- output
  forecast.result$Method <- method
  return(forecast.result)
}
