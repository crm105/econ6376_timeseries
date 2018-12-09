library(ggplot2)
library(ggfortify)
library(tseries)
library(xts)
library(dplyr)
library(tseries)
library(forecast)
#Question 1 part II

set.seed(3)
x <- w <-  rnorm(1000)

for(t in 2:1000){
  x[t] <- x[t-1] + w[t]
}

#If we believe the series to be a random walk, we would not want to
#forecast foreword. By definition, the only thing that influences the 
#future value of a random walk is a the present random term 

#Question II 

#A 

x <- c( 0, rnorm(999))
for (t in 2:1000){
  x[t] <- 3 - (.5 * x[t-1])
}

plot(x)

#As we can see, the series converges to a stationary mean at 
#a0/ (1-a1). This makes sense as the a1 parameter is < 1 

3/(1+.5)
adf.test(x)

#Simulation shows that we are not at equillibrium at x = 4
x[4]

#Next several iterations oscilate around the long run mean of 2. 
#This makes sense as we have a negative a1 parameter. 

#Question III
set.seed(4)
x <- w <- rnorm(105)

for (t in 2:105){
  x[t] <- 3 + w[t] + (.5 * w[t-1])
}

plot(x, type = "l")

#This function is called a first-order moving average or MA1 
#It is stationary, with a mean around a0 
x <- x[5:105]
mean(x)
acf(x)
var(x)
up <- mean (x) +  1.96 * (var(w) * (1 + .25))^.5
low <- mean(x) -1.96 * (var(w) * (1 + .25))^.5

plot(x, type = "l")
lines(rep(low,10000))
lines(rep(up,10000))

length (x[x> up])/length(x) * 100 +
length (x[x < low])/length(x) * 100

??ci

##Question IV ## 

df <- read.csv("ERCOT LZ North January 2014.csv")
ts <- ts(df$Settlement.Point.Price)

autoplot(ts)


#Probably stationary according to ADF and PP tests, although first half vs second half mean is 
# pretty different. May be worth investigating
adf.test(ts)
PP.test(ts)
auto.arima(ts, allowdrift = TRUE, trace = TRUE)
mean(ts[1:1500])
mean(ts[1500:2976])

#


