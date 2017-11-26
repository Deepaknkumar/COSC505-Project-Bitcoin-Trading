setwd('E:/cosc505-project/')
install.packages("DAAG")
library("DAAG")
library(readr)
library(readr)
btceu<- read_csv("E:/cosc505-project/bitcoin-historical-data/btceUSD_1-min_data_2012-01-01_to_2017-05-31.csv")
View(btceUSD_1_min_data_2012_01_01_to_2017_05_31)

btce<-btceu[complete.cases(btceu),]
hist(btce$Open,breaks=100)
csv<-read.csv('bitcoin-historical-data/btceUSD_1-min_data_2012-01-01_to_2017-05-31.csv')
csv2<-csv[complete.cases(csv), ]

open<-csv2[,2]
#open1<-head(open,100000)
qreference(open, xlab=" ",
           distribution = function(x) qexp(x, rate = 1/mean(open)) )

tstamp<-csv2$Timestamp
#tstamp1<-head(tstamp,100000)
exponential.model <- lm(log(open)~ tstamp)
summary(exponential.model)
r1<-range(tstamp)
timevalues <- seq(r1[1], r1[2], 10)
Counts.exponential2 <- exp(predict(exponential.model,list(tstamp=timevalues)))
plot(tstamp, open,pch=16)
lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")
