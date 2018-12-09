getwd()
setwd("Documents/active_projects/econ6376_timeseries/assignments")

library("tseries")
library("forecast")
library("ggplot2")
library("ggfortify")

df <- read.csv("assignment_4.csv")
df <- ts(df[,1], start = 0)



#At first glance plot appears to be pretty stationary,
#perhaps with some conditional heteroskedasticity? Any Interventions?


plot(df)
plot(diff(df))

#I'd like to remove the first 100 observations while the series is still initializing
df <- ts(df[100:1000])
autoplot(df) + theme_bw() + geom_hline(yintercept = mean(df, na.rm = TRUE), linetype = "dashed", color = "red")


# ADF and PP tests confirm stationarity. This suggests we don't need to difference

adf.test(df)
PP.test(df)

#Decay is  stronger than what you would see from a random walk
#Almost looks like a long memory process
acf(df)

#PACF shows a significant first lag second and third lags
#so an AR1  or AR2 model might be appropriate might fit well
pacf(df)


#For the below models, SSR suggests that the arma(1,1) is a best fit
# First order integrated models performed most poorly
#AIC further suggest 

arma1 <- arma(df, order = c(1,0,0) )
summary(arma1)
sum(arma1$residuals^2, na.rm = TRUE)

arma11 <- Arima(df, order = c(1,0,1), include.mean = TRUE, include.drift = FALSE)
                
summary(arma11)
sum(arma11$residuals^2, na.rm = TRUE)

arima111 <- Arima(df, order = c(1,1,1), include.mean = TRUE, include.constant = TRUE)
summary(arima111)
sum(arima111$residuals^2, na.rm = TRUE)

arima011 <- Arima (df, order = c(0,1,1), include.mean = TRUE, include.constant = TRUE)
summary(arima011)
sum(arima011$residuals^2, na.rm = TRUE)

auto.arima(df, allowdrift = TRUE, allowmean = TRUE, trace = TRUE, stepwise = FALSE)



#Residuals plots and ACFs don't really elucidate much, most have no autocorrelation
#Arma(1,0) is definitely disqualified. Given lack of parsimony and higher SSR with integrated 
# models we can throw those out too
plot(arima011$residuals)
acf(arima011$residuals)

plot(arima111$residuals)
acf(arima111$residuals)

plot(arma1$residuals)
acf(arma1$residuals, na.action = na.pass)

plot(arma11$residuals)
acf(arma11$residuals, na.action = na.pass)

#Let's test briefly an AR(2) and MA(2) These aren't the best fit 

arma02 <- Arima(df, order = c(0,0,2), include.mean = TRUE, include.constant = TRUE,
                include.drift = TRUE)
summary(arma02)

arma20 <- Arima(df, order = c(2,0,0), include.mean = TRUE, include.constant = TRUE,
                include.drift = TRUE)
summary(arma20)


# Use the parameters of the best fitting model to forecast 1000 observations 
#forward

forecast <-  forecast( arma11, h= 1000)
plot(forecast)


#Re-estimate using a training sample

train <- df[0:500]

arma1 <- Arima(train, order = c(1,0,0), include.mean = TRUE )
summary(arma1)
sum(arma1$residuals^2, na.rm = TRUE)

arma11 <- Arima(train, order = c(1,0,1), include.mean = TRUE, include.drift = FALSE)
summary(arma11)
sum(arma11$residuals^2, na.rm = TRUE)

arima111 <- Arima(train, order = c(1,1,1), include.mean = TRUE, include.constant = TRUE)
summary(arima111)
sum(arima111$residuals^2, na.rm = TRUE)

arima011 <- Arima (train, order = c(0,1,1), include.mean = TRUE, include.constant = TRUE)
summary(arima011)
sum(arima011$residuals^2, na.rm = TRUE)

auto.arima(train, allowdrift = TRUE, allowmean = TRUE, trace = TRUE, stepwise = FALSE)


#Best fitting models appear to be ARMA(1,1) and Arima (1,1,1)

test11 <- data.frame (forecast(arma11, h = 500))
test111 <- forecast(arima111, h = 500)
x = seq(from = 500, to = 999)
par(mfrow=c(2,1))  
plot(df, ylim = c(25,75)) 
lines(x = seq(from = 500, to = 999), y = test11$Point.Forecast,
      col = "red", lty = "dashed", lwd = 3)
lines( x = x, y = test11$lower[501:1000], col = "blue" )
lines( x = x, y = test11$upper[501:1000], col = "blue" )
#geom_line(aes(x = c(1,2), y = c(10,10))


plot(df, ylim=c(0, 100))  
lines(x = x, y = test111$mean,
      col = "red", lty = "dashed", lwd = 3)
lines( x = x, y = test111$lower[501:1000], col = "blue" )
lines( x = x, y = test111$upper[501:1000], col = "blue" )




length(test111$upper)
#Some Holt Winters Action 
hw <- HoltWinters(train, gamma = FALSE)
hw
plot(hw)
hw.forecast <- forecast(hw, h = 500)
plot(hw.forecast)

?HoltWinters
