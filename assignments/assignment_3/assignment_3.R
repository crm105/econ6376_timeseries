library(dplyr)

#Define our parameters
x <- w <- rnorm(1:100)
c0 <- 4
z <- c(rep(0,100)) ; z [10:11] <- 1 
a1 <- .8
a0 <- 0

# Solve our simulation
for(t in 2:100){
  x[t] <- a0 + a1*x[t-1] + c0*(z[t]) + w[t]
}

plot(as.ts(x))


#create an empty vector to house our impulse values 

irf <- c()

# Creates a loop that solves the IRF summation equation for each point in time from 1:100
for (i in 1:20){
  
  j <- i - 1
  irf[i] <- sum( c0 * z[i:1] * a1^(0:j))
  #print(a^(0:j))
  print(z[i:1])
}


#Plot the IRF. We see the IRF approaches a peak of c/(1-a1) but doesn't quite make it before 
#falling off. After intervention is turned off, IRF cuts in half each iteration approaching zero at 
# the limit. Playing with the length of the intervention determines how close we get to the long run
#intervention value before converging back to 0


plot(irf, type = "l")


fit <- lm (x~ lag(x)+ z )
fit

#short run effect is just the z parameter. Long run effect is equal to 11.42 according to the model fit
#it should actually converge to 10 if left on forever

fit$coefficients[3] / (1 - a1)

#In reality, this series does not converge to its hypothesized long run value, nor does it change the mean
# of the series. As t -> infinity, impules approaches 0, and the mean of the series converges back to its 
# pre-intervention mean.


## PART II 

x <- w <- rnorm(1000)
a0 <- 0 
a1 <- .5
time <- c()
for (t in 1:1000){
  time[t] <- t
  x[t] <- a0 + a1*t + w[t]
}

plot(as.ts(x))

#First difference

diff.x <- diff(as.ts(x))
plot(diff.x)
mean(diff.x)

#Do SSR's show that the detrend is less noisy than first difference?
ssr.diff <- sum((diff.x - mean(diff.x))^2)

#Detrend by fitting linear model

detrend <- lm(x~ time)
detrend.x <- x - detrend$fitted.values


plot(as.ts(detrend.x))
mean(detrend.x)

ssr.detrend <- sum ((detrend.x - mean(detrend.x))^2 )

#Clearly, detrending is the better choice. This makes sense intuitively as detrending fits and removes
#the time trend, whereas first difference removes autocorrelation, essentiall fits a different model



#PART III

#Read in our data and plot our series 
df <- read.delim("stockmarket.dat", sep = "")
df.ts <- as.ts(df)


#layout(1:2)
#par(mar=c(1,1,1,1))
plot(df$NewYork, xlab = "New York Exchange")
plot(df$London, xlab = "London Exchange")


#ACF/PACF Plots suggest non-stationarity
#ADF Tests and PP tests suggest the presence of no unit root in the NY series, but not the London series

library("tseries")


acf(df.ts[,7], main = "NY Stock ACF") ; pacf(df.ts[,7], main = "NY Stocl PACF"); 
adf.test(df.ts[,7]); PP.test(df.ts[,7])

acf(df.ts[,3], "London Stock ACF") ; pacf(df.ts[,3], "London Stock PACF") ; adf.test(df.ts[,3]); PP.test(df.ts[,3])





#VAR(1) Model

library("vars")
x <- df.ts[,3]; y<- df.ts[,7]

var1 <- VAR(cbind(x,y), p=1)
var1
coef(var1)

acf(resid(var1)[, 1], main = "VAR(1) London VAR Residual ACF ")
acf(resid(var1)[, 2], main = "VAR(1) NY VAR Residual ACF")

#Optimized VAR model using R's ar function

var.ar <- ar(cbind(x,y))
var.ar
var.ar$ar

acf(var.ar$res[-c(1:12), 1], "VAR(12) London VAR Residual ACF")
acf(var.ar$res[-c(1:12), 2], "VAR(12) NY VAR Residual ACF")


#AIC for both models suggest the VAR(12) model is a better fit
AIC(var1)
sum (var.ar$aic)

#Repeat steps with Amsterdam and Frankfurt


plot(df$Frankfurt, xlab = "Frankfurt")
plot(df$Amsterdam, xlab = "Amsterdam")


#ACF/PACF Plots suggest non-stationarity
#ADF Tests and PP tests suggest the presence of no unit root in the NY series, but not the London series

library("tseries")


acf(df.ts[,1], main = "ACF Amsterdam Stock" ) ; pacf(df.ts[,1], "PACF Amsterdam Stock"); 
adf.test(df.ts[,1]) ; PP.test(df.ts[,1])

acf(df.ts[,2], main = "ACF Frankfurt Stock") ; pacf(df.ts[,2], main = "PACF Frankfurt Stock");
adf.test(df.ts[,2]); PP.test(df.ts[,2])



#VAR(1) Model

y <- df.ts[,1]; x<- df.ts[,2]

var1 <- VAR(cbind(x,y), p=1)
var1
coef(var1)

acf(resid(var1)[, 1], main = "ACF: VAR(1) Residuals Amsterdam Stock")
acf(resid(var1)[, 2], main= "ACF: VAR(1) Residuals Frankfurt Stock")

var.ar <- ar(cbind(x,y))
var.ar
var.ar$ar

acf(var.ar$res[-c(1:12), 1], main = "ACF: VAR(12) Residuals Amsterdam Stock")
acf(var.ar$res[-c(1:12), 2], main = "ACF: VAR(12) Residuals Frankfurt Stock")

AIC(var1)
sum (var.ar$aic)





