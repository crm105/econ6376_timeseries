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
#bra$price <- log(bra$price)

#US soybean prices per bushel 
us <- Quandl("TFGRAIN/SOYBEANS")
us <- us [,1:2]
colnames(us) <- c("date","price")
us$origin <- "USA"
#us$price <- log(us$price)

#This ensures our data covers the same lengths of time
bra <- bra[bra$date %in% us$date,]
us <- us[us$date %in% bra$date,]


#Merge Frames. df.wide houses data in wide format 
df <- rbind(us, bra)
df <- spread(df, origin, price)
df$spread <- df$USA - df$BRA
df <- na.omit(df)

plot(df$spread, type = "l")
test<- df$USA
#Some Plots 

graphics.off()
par("mar")
par(mar=c(1,1,1,1))
# layout(1:2)
plot(bra$date, bra$price,  type = "line")
plot(us$date, us$price,  type = "line")


#This plot shows the two price series and spread. It's pretty clear that we see significant co-movement
ggplot(df, aes(x = date, y = USA ))+
  theme_bw()+ geom_line(col = "red")+
  #geom_line(aes(y = df$BRA), col = "blue")+ 
  geom_line(aes(y = df$spread))+
  
  labs(title="US Soybean Prices"  , subtitle="Per 60 KG", y="", x="",
       caption="*Black series denotes spread relative to Brazilian soybeans. Verticle line signals intervention")+
  theme(plot.title = element_text( face = "bold", size = "19.5"))+
  theme(plot.subtitle = element_text( face = "italic", size = "12"))+
  theme(axis.text.x =element_text(size=13.5, vjust = -.75)
        , axis.text.y = element_text(size = 13.5 ),
        legend.title=element_text(face = "bold",size=11), 
        legend.text=element_text(size=10, face = "bold"),
        plot.caption = element_text (size = 12))+
   geom_vline(xintercept=as.Date("2018-04-01"), linetype = "dashed")





df$intervention <- ifelse(df$date > "2018-04-03", 1, 0)

#Firt Arima to pre-intervention 
ts <- ts(df$USA)
ts.pre<- ts(df[df$date < "2018-04-03",]$USA)
auto.arima(ts.pre)

#Since its a random walk we need to take the first diff

diff.ts <- diff(ts)
auto.arima(diff.ts)


adf.test(df$USA)
Arima(df$USA, order = c(1,0,0) )
acf(df$USA)

auto.arima(diff.ts, xreg = diff(df$intervention))

#Fit model to pre-intervention data
ts <- as.ts(df$USA)
ts.pre <- as.ts(df[df$date = "2018-05-25",]$spread)

ts.pre.arima <- auto.arima(ts.pre, allowdrift = TRUE, allowmean = TRUE, trace= TRUE)
summary(ts.pre.arima)

ts.pred <- predict(ts.pre.arima, n.ahead = 173 )
plot(ts.pred$predict)
plot(ts[(4337-173):4336], type = "l")

diff <- as.ts(ts.pred$pred -ts[(4337-173):4336])
plot(diff)
length(ts[(4337-173):4336])

auto.arima(df$USA, allowdrift = TRUE, allowmean = TRUE, xreg = (df$intervention))

summary(arima)
?Arima

plot(df$spread, type = "l")
lines(arima$fitted, type = "l", color = "red")

plot(arima$fitted, type = "l", col = "red")
plot(arima$residuals)
