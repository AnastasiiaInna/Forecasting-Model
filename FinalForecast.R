# --- GFinal forecast --- #

source(paste0(source.dir, "Function_Model_Forecasting.R"))

metrics   <- metrics.summary
method    <- eval(parse(text = paste("sqldf('select", paste(group.by, collapse=","), ", Method, min(MASE) from metrics group by", paste(group.by,collapse=","),"')",collapse="")))
data.good <- join(data.good, method, by = group.by)

data.ets <- subset(data.good, Method %in% c("STL_ETS"))
data.snaive <- subset(data.good, Method %in% c("STL_snaive"))
data.arima <- subset(data.good, Method %in% c("STL_ARIMA"))
data.reg <- subset(data.good, Method %in% c("STL_REG"))

output.ets <- data.frame()
output.snaive <- data.frame()
output.arime <- data.frame()
output.reg <- data.frame()

output.ets    <- eval(parse(text = paste("ddply(data.ets, .(", as.name(paste(group.by,collapse=",")), "), forecast.result, method='STL_ETS')",sep="")))
output.snaive <- eval(parse(text = paste("ddply(data.snaive, .(", as.name(paste(group.by,collapse=",")), "), forecast.result, method='STL_snaive')",sep="")))
output.arima  <- eval(parse(text = paste("ddply(data.arima, .(", as.name(paste(group.by,collapse=",")), "), forecast.result, method='STL_ARIMA')",sep="")))
output.reg    <- eval(parse(text = paste("ddply(data.reg, .(", as.name(paste(group.by,collapse=",")), "), forecast.result, method='STL_REG')",sep="")))

output.all    <- rbind(output.ets, output.snaive, output.arima, output.reg)

output.all$Period <- NULL 
output.all$lo95<-NULL
output.all$hi95<-NULL
output.all$lo80<-NULL
output.all$hi80<-NULL

final.forecasting <- aggregate(forecast ~ Date, output.all, FUN = sum)
final.forecasting$forecast <- as.integer(final.forecasting$forecast)
colnames(final.forecasting)[2] <- "Volume"

history.data.aggregated.by.date <- aggregate(Volume ~ Date, data, FUN = sum)

history.forecasting <- rbind(history.data.aggregated.by.date, final.forecasting)

print(ggplot(history.forecasting, aes(Date, Volume)) + geom_line() +  scale_x_date(breaks = date_breaks("6 months"), labels=date_format("%b %y")) + xlab("") + geom_text(aes(label = Volume), size=3, vjust = -0.2, hjust = -0.2))
