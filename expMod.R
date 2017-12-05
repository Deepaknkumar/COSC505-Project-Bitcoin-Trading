# Simulating Exponential model

library(readr)
bt <- read_csv("D:/deepak/UBC Courses/Modeling and Simulation/Project/btceUSD_1-min_data_2012-01-01_to_2017-05-31.csv")  # reading the bitstampUSD dataset
bt <- bt[complete.cases(bt),] # reading the bitstampUSD dataset
NSamples <- 100000            # number of samples 
bts <- bt[sample(length(bt$Timestamp),NSamples),]    # selecting random sample points

Open <- bts$Open
mean.Open <- mean(Open)     # mean of the opening values of the bitcoin
sim.Open <- rexp(NSamples,rate = 1/mean.Open) # Simulate new observations
sim.Open
#plot(Open~bts$Timestamp,col=1)

plot(sim.Open~bts$Timestamp,col=4) # plotting new observations
hist(sim.Open,freq = FALSE)      
hist(Open, freq= FALSE)   # density of the Opening values
curve(dexp(x,rate=1/mean.Open),add=TRUE)    # overlaying the exponential curve
