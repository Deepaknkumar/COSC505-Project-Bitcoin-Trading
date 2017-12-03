setwd('E:/cosc505-project/')
install.packages("DAAG")
library("DAAG")

csv<-read.csv('bitcoin.csv')
csv2<-csv[complete.cases(csv), ]

open<-csv2[,2]

# Plotting qrefrence plots for checking if our data follows an exponential model
qreference(open, xlab=" ",
           distribution = function(x) qexp(x, rate = 1/mean(open)) )

tstamp<-csv2$Timestamp

exponential.model <- lm(log(open)~ tstamp)
summary(exponential.model)
r1<-range(tstamp)
timevalues <- seq(r1[1], r1[2], 10)
Counts.exponential2 <- exp(predict(exponential.model,list(tstamp=timevalues)))
plot(tstamp, open,pch=16)
lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")
