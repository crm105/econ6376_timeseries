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
for (t in 2:1000)
  {
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

#Lets extract the variance. First we need to pre-whiten (demean)

acf(ts)
var.ts <- ts - mean(ts)
mean(var.ts)

#now we can create the variance series
var.ts <- (ts - mean(ts))^2

#Check for stationarity 
autoplot(var.ts)

#ACF suggest we don't have a unit root, but definitely autocorrelation
acf(var.ts)

#ADF.tests and PP show stationarity as well
adf.test(var.ts)
pp.test(var.ts)

#Fit a model to explain our series
arima.ts <- auto.arima(ts)
summary(arima.ts)

acf(arima.ts$residuals^2)


a.garch1 <- garch(arima.ts$residuals, order=c(0,1), grad="numerical",
                  trace=FALSE)
summary(a.garch1)
acf(a.garch1$residuals[-1])



a.garch2 <- garch(ts, order=c(0,1), grad="numerical",
                 trace=FALSE)

summary(a.garch2)
confint(a.garch1)

acf(a.garch1$residuals, na.action = na.pass)



# PART V #

wti <- read.csv("wti_prices.csv")
names(wti)[2] <- "wti"
wti$DATE <- as.Date(wti$DATE, format = "%Y-%m-%d")

brent <- read.csv("brent_prices.csv")
brent$DATE <- as.Date(brent$DATE, format = "%Y-%m-%d")

names(brent)[2] <- "brent"

df <- merge(brent, wti)
df <- na.omit(df)
df$spread <- df$wti - df$brent
df <- df[df$DATE < "2018-01-01",]

ggplot(df, aes(x = DATE, y = wti))+ theme_bw()+
geom_line(color = "red")+ geom_line(aes(y = brent), color = "blue")

#Let's test each series order of integration 
#1987-2018 Range
  #Definitely appear to have a similar acf decay structure
  #Both suggestive of random walk. Near identical PACF as well
layout(1:2)
acf(df$wti); acf(df$brent)
pacf(df$wti); pacf(df$brent)

#ADF and PP tests further suggest we are I(1) for both series
adf.test(df$wti); adf.test(df$brent)
PP.test(df$wti); PP.test(df$brent)

 #First differences return both series to stationary
plot(diff(df$wti));
plot(diff(df$brent))

#residuals of differenced series appear stationary. This 
#suggests co-integration; with the clear exception of the 
#post intervention period . PO test confirms whole series is 
#co-integrated

plot(df$spread)

po.test(cbind(df$brent, df$wti))

#2005-2017 Range
df1 <- df[df$DATE > "2004-12-31",]

layout(1:2)
acf(df1$wti); acf(df1$brent)
pacf(df1$wti); pacf(df1$brent)

#ADF and PP tests further suggest we are I(1) for both series
adf.test(df1$wti); adf.test(df1$brent)
PP.test(df1$wti); PP.test(df1$brent)

#First differences return both series to stationary
plot(diff(df1$wti));
plot(diff(df1$brent))

#residuals of differenced series appear stationary. This 
#suggests co-integration; with the clear exception of the 
#post intervention period . PO test confirms #the post 2007
# series is not co-integrated

plot(df1$spread)
po.test(cbind(df1$brent, df1$wti))

plot(df1$spread)

#Create the intervention term.
df1$decline <- 0
df1[df1$DATE > "2010-11-01" & df1$DATE < "2012-06-01",]$decline = 1

df1$reversal <- 0
df1[df1$DATE > "2012-05-19",]$reversal = 1

df1$time <- seq(nrow(df1))
df1$post.int <- df1$time - df1[df1$DATE == "2012-05-01",]$time
df1[df1$post.int < 1,]$time <- 0

# df1$t.pre.int <- df1$time - df1[df1$DATE == "2010-11-01",]$time
# df1[df1$post.int < 1,]$time <- 0
# df1$pre.interaction <- df1$t.pre.int * df1$pre.intervention



#Plotting the suggested intervention date shows that the divergence
#began much sooner. We should model this too.

ggplot(df1, aes(x = DATE, y = wti))+ theme_bw() + geom_line()+
geom_vline(xintercept = as.Date("2012-10-01"), color = "red", linetype="dashed")+
geom_vline(xintercept = as.Date("2010-11-01"), color = "blue", linetype="dashed")

#Baseline Regression 

lm <- lm(df1$wti~df1$brent)
summary(lm)
plot(df1$wti, type = "l"); lines(lm$fitted.values, type = "l", col="red")



# Baseline with time trend
lm1 <- lm(df1$wti ~ df1$brent + df1$time)
summary(lm1)

#Baseline with time trend and reversal dummy 
lm2 <- lm(df1$wti ~ df1$brent + df1$time + df1$reversal)
summary(lm2)

#Baseline with time trend reversal dummy and interaction between time 
#and reversal
lm3 <- lm(df1$wti ~ df1$brent + df1$time + df1$reversal + df1$post.int)
summary (lm3)

#lm3 + decline intervention term 
lm4 <-lm(df1$wti ~ df1$brent + df1$time + df1$reversal +
           df1$decline+df1$post.int)
summary (lm4)
#Run some regressions with our initial intervention date 

reg1 <- lm(df1$wti~df1$brent +df1$decline+df1$reversal)
summary(reg1)
plot(df1$wti, type = "l"); lines(reg1$fitted.values, type = "l", col = "red")


#First model fit suggests intervention reduced spread by 4.6 dollars
#t-value (-5.43); Fstat = 1385? 

#Fit with time trend shows time trend is not significant. We can get more
#creative

reg1.1 <- lm(df1$wti~df1$brent + df1$reversal +
               df1$time)
summary(reg1.1)
plot(reg1.1$residuals, type = "l")


reg1.2 <- lm(df1$wti~ df1$brent  +
               df1$reversal + df1$post.int )
               
summary(reg1.2)
plot(reg1.2$residuals, type = "l")


#####################################################################
#####################################################################

#####################################################################
#####################################################################

#####################################################################
#####################################################################

#Fit an ARIMA model to the pre-intervention period. Model jump + slope
#shift 

pre <- df1[df1$DATE < "2012-05-01",]
xreg.pre <- pre %>% select(brent)


pre.arima1 <- auto.arima(pre$wti, allowdrift = TRUE,
                     allowmean = TRUE, xreg = xreg.pre, trace = TRUE)
summary(pre.arima1)

#Fit ARIMAX with intervention * time interaction

xreg <- df1 %>% select( reversal,brent , decline)
arima1 <- auto.arima(df1$wti, allowmean = TRUE, xreg = xreg)
  summary(arima1)
plot(arima1$residuals, type = "l")

plot(df1$wti, type = "l")
lines(arima2$fitted, col = "red")


arima2 <- auto.arima(df1$wti , allowmean = FALSE, allowdrift = TRUE,
                     xreg = xreg)
summary(arima2)
plot(df1$spread, type = "l")

1.7421/(1-.7733)


