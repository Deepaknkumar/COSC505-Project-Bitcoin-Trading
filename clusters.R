## clusters in dataset

library(readr)
bt <- read_csv("bitstampUSD_1-min_data_2012-01-01_to_2017-10-20.csv")
NSamples <- 100000
bts <- bt[sample(length(bt$Timestamp),NSamples),]

# opening times
xopen <- cbind(TimeStamp = bts$Timestamp, Open = bts$Open)
cl_open <- kmeans(xopen,2)
plot(xopen,col=cl_open$cluster)

#closing times
xclose <- cbind(TimeStamp = bts$Timestamp, Close = bts$Close)
cl_close <- kmeans(xclose,2)
plot(xclose,col=cl_close$cluster)

# Low
xlow <- cbind(TimeStamp = bts$Timestamp, Low = bts$Low)
cl_low <- kmeans(xlow,2)
plot(xlow,col=cl_low$cluster)

# High
xhigh <- cbind(TimeStamp = bts$Timestamp, high = bts$High)
cl_high <- kmeans(xhigh,2)
plot(xhigh,col=cl_high$cluster)

# All of them are same!