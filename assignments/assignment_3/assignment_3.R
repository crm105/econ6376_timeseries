set.seed(2)

y <- w <- rnorm(100, sd = 1)
a0 <- 1
a1 = .5
c = 1



#create a dummy z that turns on at 250

z <- rep(0,100)
z[50:52] <- 1
  


for (t in 2:100)
{y[t] <- a0 + a1*y[t-1] + c*z[t] 
+ w[t]}

plot(y)

library(ggplot2)
library(ggfortify)
autoplot(as.ts(y))

y[53]

#IRF Plot
options(digits=20)
alpha1 <- 0.5
cee0 <- 1
irf <-rep(cee0, 20)
for (t in 2:20)
{irf[t] <- irf[t-1]+cee0*alpha1^(t-1)}
irf
plot(irf, type="l", xlab="Time", ylab="",
     main="IRF(alpha1=0.5, c=1)" )
