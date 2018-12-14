# TITLE: Price Series Analysis 
#Author: Christopher Montgomery
#Last Revised: 12/12/2018


#Here are some libraries we will be needing

library("Quandl")
library("dplyr")
library("ggplot2")
library("lubridate")
library("ggfortify")
library("tidyr")
library("tseries")
library("forecast")
library("ggfortify")




#Brazil soybean prices per 60kg bag USD

Quandl.api_key("1G_nWZYYtdegKrybsgz4")
bra <- Quandl("CEPEA/SOYBEAN")
bra <- bra[,1:2]
colnames(bra) <- c("date", "price")
bra$origin <- "BRA"


#bushel of soybeans = 60 pounds. 1 KG = 2.20462 pounds Convert brazil prices to bushels
bra$price <- bra$price / 2.20462
bra$price <- log(bra$price)

#US soybean prices per bushel 
us <- Quandl("TFGRAIN/SOYBEANS")
us <- us [,1:2]
colnames(us) <- c("date","price")
us$origin <- "USA"
us$price <- log(us$price)

#This ensures our data covers the same lengths of time
bra <- bra[bra$date %in% us$date,]
us <- us[us$date %in% bra$date,]


#Merge Frames. df.wide houses data in wide format 
df <- rbind(us, bra)
df <- spread(df, origin, price)
df$spread <- df$USA - df$BRA
df <- na.omit(df)




#This plot shows the two price series and spread. It's pretty clear that we see significant co-movement

ggplot(df, aes(x = date, y = USA ))+
  theme_bw()+ geom_line(col = "red")+
  #geom_line(aes(y = df$BRA), col = "blue")+ 
  geom_line(aes(y = df$spread))+
  
  labs(title="US Soybean Prices"  , subtitle="Per 60 KG", y="", x="",
       caption="*Black series denotes spread relative to Brazilian soybeans. Verticle line signals intervention\n Source: CBOT")+
  theme(plot.title = element_text( face = "bold", size = "19.5"))+
  theme(plot.subtitle = element_text( face = "italic", size = "12"))+
  theme(axis.text.x =element_text(size=13.5, vjust = -.75)
        , axis.text.y = element_text(size = 13.5 ),
        legend.title=element_text(face = "bold",size=11), 
        legend.text=element_text(size=10, face = "bold"),
        plot.caption = element_text (size = 11, vjust = 5))+
   geom_vline(xintercept=as.Date("2018-04-01"), linetype = "dashed")





df$intervention <- ifelse(df$date > "2018-04-03", 1, 0)
df$time <- seq(nrow(df))
df$time <- df$time - df[df$date == "2018-04-03",]$time
df[df$time < 1,]$time <- 0

xreg <- df[c("intervention", "time")]

#Firt Arima to pre-intervention 

ts <- ts(df$USA)
ts.pre<- ts(df[df$date < "2018-04-03",]$USA)
pre.arima <- auto.arima(ts.pre, allowdrift = TRUE, allowmean = TRUE,
                    trace = TRUE)
summary(pre.arima)
autoplot(acf(pre.arima$residuals))

pre.arima2 <- Arima(ts.pre, order = c(1,1,1), include.mean = TRUE, include.constant = TRUE)
summary(pre.arima2, na.action = na.pass) 
autoplot(acf(pre.arima2$residuals))
plot(pre.arima2$residuals, type = "l")

arima3 <- Arima(ts.pre, order = c(1,1,0), include.mean = TRUE, include.constant = TRUE)
summary(pre.arima3) 
autoplot(acf(pre.arima3$residuals))

arima4 <- Arima(ts.pre, order = c(1,1,1), include.mean = TRUE, include.constant = TRUE)
summary(pre.arima4) 
autoplot(acf(pre.arima4$residuals))



#Apply arima(0,1,0) to full series including exogenous regressors

full.arima <- Arima(df$USA, order = c(0,1,0),include.mean = TRUE,
                    include.drift = TRUE, xreg = (xreg))

summary(full.arima)


plot(df$spread, type = "l")
lines(arima$fitted, type = "l", color = "red")

plot(arima$fitted, type = "l", col = "red")
plot(full.arima$residuals)
acf(full.arima$residuals)



