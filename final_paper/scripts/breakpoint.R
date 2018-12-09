#Author- Christopher Montgomery
#Last Revised- 11/30/2018
#Description- The Following script conducts a break point analysis to determine
#the most appropriate break point for the US-Brazil soybean spread. 


#Here are some libraries we will be needing

library("Quandl")
library("dplyr")
library("ggplot2")
library("lubridate")
library("ggfortify")
library("tidyr")
library("tseries")
library("forecast")
library("strucchange")



#Brazil soybean prices per 60kg bag USD

Quandl.api_key("1G_nWZYYtdegKrybsgz4")
bra <- Quandl("CEPEA/SOYBEAN")
bra <- bra[,1:2]
colnames(bra) <- c("date", "price")
bra$origin <- "BRA"


#bushel of soybeans = 60 pounds. 1 KG = 2.20462 pounds Convert brazil prices to bushels
bra$price <- bra$price / 2.20462

#US soybean prices per bushel 
us <- Quandl("TFGRAIN/SOYBEANS")
us <- us [,1:2]
colnames(us) <- c("date","price")
us$origin <- "USA"

#This ensures our data covers the same lengths of time
bra<- bra [bra$date >= tail(us$date,1),]
bra<- bra [bra$date <= head(us$date,1),]
us<- us [us$date >= tail(bra$date,1),]
us<- us [us$date <= head(bra$date,1),]

#Merge Frames. df.wide houses data in wide format 
df <- rbind(us, bra)
df <- spread(df, origin, price)
df$spread <- df$USA - df$BRA
df <- na.omit(df)

df <- df[df$date > "2018-01-01",]

#Lets do an endogenous chow test of the spread series to determine where the spread
# begins to widen 

library(strucchange)
ts.spread <- as.xts(df$spread, order.by = df$date)

# store the breakdates
ts <- as.ts(df$spread)
bp_ts <- breakpoints(ts~1)
arima <- auto.arima(ts)
bp_ts <- breakpoints(arima$fitted~1)


# this will give you the break dates and their confidence intervals
summary(bp_ts) 

# store the confidence intervals
ci_ts <- confint(bp_ts)

## to plot the breakpoints with confidence intervals
plot(ts)
lines(bp_ts)
lines(ci_ts)

df[c(bp_ts$breakpoints),]

bp_ts$breakpoints


plot(bp_ts)
plot(df$spread, type = "l")
lines(c(bp_ts$breakpoints))
abline(v = bp_ts$breakpoints)
summary(bp_ts)

arima.spread <- auto.arima(df$spread)
summary(arima.spread)

lag(ts)
ts
diff(ts)


