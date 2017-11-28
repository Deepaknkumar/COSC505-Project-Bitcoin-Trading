#Written by Shima Amirsadri
setwd('E:\\cosc505-project\\')
install.packages('tseries')
install.packages('forecast')
library('ggplot2')
library('forecast')
library('tseries')
allow.multiplicative.trend=TRUE 
btc<- read.csv("bitcoin.csv",header=TRUE, stringsAsFactors=FALSE)

#Replacing the Nan opening values with the nearest values 
btc$openC<-replaceNanNearst(btc$Open)
#Changing the interval from minute to minute to day to day
sq<-seq(1,length(btc$openC),by = 1440)
btc_date<-btc$Timestamp[sq]
BTC<- data.frame(btc_open=btc$openC[sq],btc_Date=as.Date(as.POSIXct(btc_date, origin = "1970-01-01", tz = "GMT")))
btc_Date


#Analyzing the data
ggplot(BTC, aes(btc_Date, btc_open)) + geom_line() + scale_x_date("Year")+ ylab("Daily bitcoin openning value") +
  xlab("Year")

#Removing the outliers
count_ts = ts(BTC[, c('btc_open')])
BTC$clean_open = tsclean(count_ts)
ggplot() +
  geom_line(data = BTC, aes(x = btc_Date, y = clean_open)) + ylab("Daily bitcoin openning value")

#Making the clean data(The data with no outliers) smoother using the moving average 
BTC$open_ma = ma(BTC$clean_open, order=7) 
BTC$open_ma30 = ma(BTC$clean_open, order=30)
BTC$open_ma120 = ma(BTC$clean_open, order=120)

ggplot() +
  geom_line(data = BTC, aes(x = btc_Date, y = clean_open, colour = "real data")) +
  geom_line(data = BTC, aes(x = btc_Date, y = open_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = BTC, aes(x = btc_Date, y = open_ma30, colour = "Monthly Moving Average"))  +
  geom_line(data = BTC, aes(x = btc_Date, y = open_ma120, colour = "Seasonal Moving Average"))  +xlab("Date")+
  ylab("Bitcoin opening value")

#Decomposion
count_ma = ts(na.omit(BTC$open_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_open <- seasadj(decomp)
plot(decomp)

#ADF Test for testing stationarity
adf.test(count_ma, alternative = "stationary")

#Plotting ACF and PACF plots for displaying the auto correlation
Acf(count_ma, main='')
Pacf(count_ma, main='')

#Plotting the differenced series for choosing the order of differencing
count_d1 = diff(deseasonal_open, differences = 2)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

#Plotting ACF and PACF plots of the differencing series to choose p and q for the model
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#Fitting the model
auto.arima(deseasonal_open, seasonal=FALSE)

#Testing the model using model residuals
fit<-auto.arima(deseasonal_open, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(2,2,2) Model Residuals')

#Fitting the model with new parameters
fit2 = arima(deseasonal_open, order=c(2,2,7))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
arima(x = deseasonal_open, order = c(2, 2, 7))

#Forecasting
fcast <- forecast(fit2, h=30)
plot(fcast)

#Testing the performance of the model
hold <- window(ts(deseasonal_open), start=1740)

fit_no_holdout = arima(ts(deseasonal_open[-c(1740:1764)]), order=c(2,2,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=24)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_open))


# deseasonal_open
# fit_w_seasonality = auto.arima(deseasonal_open, seasonal=TRUE)
# fit_w_seasonality
# 
# replaceNanNearst <- function(dat) {
#   N <- length(dat)
#   na.pos <- which(is.na(dat))
#   if (length(na.pos) %in% c(0, N)) {
#     return(dat)
#   }
#   non.na.pos <- which(!is.na(dat))
#   intervals  <- findInterval(na.pos, non.na.pos,
#                              all.inside = TRUE)
#   left.pos   <- non.na.pos[pmax(1, intervals)]
#   right.pos  <- non.na.pos[pmin(N, intervals+1)]
#   left.dist  <- na.pos - left.pos
#   right.dist <- right.pos - na.pos
#   
#   dat[na.pos] <- ifelse(left.dist <= right.dist,
#                         dat[left.pos], dat[right.pos])
#   return(dat)
# }