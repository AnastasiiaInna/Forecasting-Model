
input.data.fielname <- "zadanie_planeta.csv"
regressor.filename <- "Regressor.csv"

regressor <- read.csv(regressor.filename)
regressor$Date <- ymd(regressor$Date)

data <- read.csv(input.data.fielname)

summary(data)
# apply(data, 2, typeof)

data$date <- ymd(dmy(data$date))
colnames(data) <- c("Date", "Hour", "Volume")

data <- data %>% mutate(Date = ymd(paste(year(Date), month(Date), "01", "-")))
data <- aggregate(Volume ~., data, FUN = sum)

summary(data)
data <- data[data$Date != ymd(20161001), ]

# ---------- Forecast Parameters ---------- #

group.by <- c("Hour")

forecast.length <- 12
test.length <- 7

# seasonal cycle
seasonality <- 12 # 12

# time unit of observation
observation.freq <- "Day"
timeformat <- "%Y-%m-%d"

min.length <- 2 * seasonality
value.threshold <- 0
date.threshold <- as.Date("2016-01-01")
date.latest <- as.Date("2016-09-01")
date.start <- as.Date("2014-01-01")

# start point that doestn't involve abnormal trend
date.start.correct <- as.Date('2014-01-01')

# Parameters Table
input<- data.frame(test.length = test.length, seasonality = seasonality, 
                   observation.freq = observation.freq, timeformat = timeformat,
                   stringsAsFactors = FALSE
)
# ------------------------------------------ #

data.good <- data

