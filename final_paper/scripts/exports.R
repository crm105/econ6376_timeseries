#getwd()
#setwd("Documents/active_projects/econ6376_timeseries/final_paper")

library("readxl")
library("zoo")
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(xts)
library(TSA)
#library(data.table) 

df <- read.csv("data/exports.csv", skip =6)



#Clean-up our columns a bit keeping only the ones were interested in, convert date to date 
keep <- c("Date", "Country", "Exports", "Sales.2", "Sales.4")
df <- df[,keep]
colnames (df) <- c("date", "country", "exports","n.sales.cmy", "n.sales.nmy")
df$date <- as.Date(df$date, format = "%m/%d/%Y")
df <- df[df$date > "1998-12-31",]
integers <- c("exports", "n.sales.cmy", "n.sales.cmy", "n.sales.nmy")

for (i in integers){
  df[,i] <- as.numeric (gsub(",", "", df[,i]))

}
df$sales <- df$n.sales.cmy + df$n.sales.nmy



#Convert to monthly data and take the logs
 df <- df %>%
dplyr :: select( date, country, exports, sales)  %>%
dplyr :: group_by( date = floor_date(date, "months"),country) %>%
dplyr ::summarise(exports = sum(exports), sales = sum(sales))
 
 # df[df$exports < 1,]$exports <- 1
 # df$exports <- log(df$exports)

 #Drop non-country rows that are not relevant 
 
 df<- df[!df$country %in% c("KNOWN", "UNKNOWN"),]


#Create new frames with total and three major trade partners. Might Not Keep This. Am i missing a country? 

EU <- c("AUSTRIA", "BULGARIA", "BELGIUM-LUXEMBOURG","DENMARK", 
        "CYPRUS", "CZECHIA", "ESTONIA","FINLAND", "FRANCE","GERMANY", "GREECE",
        "HUNGARY", "IRELAND", "ITALY", "LATVIA", "LITHUANIA", "MALTA","NETHERLANDS",
        "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA", "SPAIN","SWEDEN", "UNITED KINGDOM"
        )
#Confirm the individual  EU countries sum to EU 27 and values are not repeated. This checks out

sum (df[df$country == "EUROPEAN UNION - 27",]$exports) -  sum (df[df$country %in% EU,]$exports)
sum(df[df$country == "GRAND TOTAL",]$exports) - sum(df[!df$country == "GRAND TOTAL", ]$exports) 

#Keep only values we want. Drop November until data through end of the month is 
#updated 


df <- group_by(df, date) %>% 
    filter(country %in% c("GRAND TOTAL", "EUROPEAN UNION - 27", "CHINA, PEOPLES REPUBLIC OF" ))
#df <- df[df$date < "2018-11-01",]



#Generate the intervention dummy. Let's try a gradual change 

df$intervention <- 0
df[df$date > "2018-03-01",]$intervention = 1

# df$k12 <- 0
# df[df$date > "2016-12-31" & df$date < "2018-01-01",]$k12 <- 1



chn <- df[df$country == "CHINA, PEOPLES REPUBLIC OF",]
plot(chn$exports, type = "l")
eu <- df[df$country == "EUROPEAN UNION - 27",]

chn$time <- seq(nrow(chn))
chn$time <- chn$time - chn[chn$date == "2018-04-01",]$time


xreg <- data.frame(chn$intervention, (chn$intervention *chn$time) )


###########################################################################

#Lets fit a best arima to the total exports to china pre-intervention period


ts.chn <- ts(chn$exports,   frequency = 12 )

ts.chn.pre <- ts(chn[chn$date < "2018-04-01",]$exports,   frequency = 12 )
arima.chn.pre <- auto.arima (ts.chn.pre, trace = TRUE, seasonal = TRUE, 
                         allowdrift = TRUE,  allowmean = TRUE)
summary(arima.chn.pre)
autoplot(acf(arima.chn.pre$residuals))

arima.chn.pre1 <- Arima (ts.chn, order = c(1,0,0), seasonal = c(1,1,2),
                         include.constant = TRUE  , include.mean = TRUE, include.drift = TRUE)
summary(arima.chn.pre1)
autoplot(acf(arima.chn.pre1$residuals))




ggplot(chn[chn$date < "2018-04-01",], aes(x = date, y = exports))+ 
  theme_classic()+ geom_line()+
  geom_line(aes(y = arima.chn.pre$fitted), col = "red")+
  
  labs(title="China: Pre-intervention  Exports Realized vs. Fitted"  , subtitle="SARIMA (1,0,0)(0,1,2)", y="", x="",
       caption="Red = Fitted Values")+
  theme(plot.title = element_text( face = "bold", size = "19.5"))+
  theme(plot.subtitle = element_text( face = "italic", size = "12"))+
  theme(axis.text.x =element_text(size=13.5, vjust = -.75)
        , axis.text.y = element_text(size = 13.5 ),
        legend.title=element_text(face = "bold",size=11), 
        legend.text=element_text(size=10, face = "bold"),
        plot.caption = element_text (size = 12))


#Step 2 Fit forecast from fitted values to realized post-intervention values
pred.chn.pre <- forecast :: forecast(arima.chn.pre, h = 8)

plot(pred.chn.pre)
diff.chn <- pred.chn.pre$mean - ts.chn[88:95]

autoplot(diff.chn) + theme_bw()+ 
labs(title="China: Sarima Model Forecasted Exports vs. Realized Post-Intervention"  , subtitle="SARIMA (1,0,0)(0,1,0)", y="", x="",
caption=" Y = Forecasted Values - Realized Values")+
  theme(plot.title = element_text( face = "bold", size = "19.5"))+
  theme(plot.subtitle = element_text( face = "italic", size = "12"))+
  theme(axis.text.x =element_text(size=13.5, vjust = -.75)
        , axis.text.y = element_text(size = 13.5 ),
        legend.title=element_text(face = "bold",size=11), 
        legend.text=element_text(size=10, face = "bold"),
        plot.caption = element_text (size = 12))



summary(arima.chn.pre)


#Step 3- Fit model to entire China Series

arima.chn <- Arima(ts.chn, include.mean = TRUE,include.constant = TRUE, order = c(1,0,0), seasonal = c(0,1,2), 
 xreg = xreg)

#arima.chn <- auto.arima(ts.chn, allowdrift = TRUE, allowmean = TRUE, xreg =chn$intervention)


summary(arima.chn)

autoplot(arima.chn$residuals)+ theme_bw()+
labs(title="China: Intervention Analysis Residuals"  , subtitle="SARIMA (1,0,0)(0,1,2)", y="", x="",
     caption=" Y = Forecasted Values - Realized Values")+
  theme(plot.title = element_text( face = "bold", size = "19.5"))+
  theme(plot.subtitle = element_text( face = "italic", size = "12"))+
  theme(axis.text.x =element_text(size=13.5, vjust = -.75)
        , axis.text.y = element_text(size = 13.5 ),
        legend.title=element_text(face = "bold",size=11), 
        legend.text=element_text(size=10, face = "bold"),
        plot.caption = element_text (size = 12))


acf(arima.chn$residuals)




######################################################


#Model fit for European Union pre-intervention 
ts.eu <- ts(eu$exports, frequency = 12)
ts.pre.eu <- ts(eu[eu$date < "2018-04-01",]$exports, frequency = 12)
#arima.eu.pre <- auto.arima (ts.pre.eu, trace = TRUE, seasonal = TRUE, allowdrift = TRUE,
#                            allowmean = TRUE)

arima.eu.pre <- Arima (ts.pre.eu, include.mean = TRUE, include.drift = TRUE,
               order = c(2,0,3), seasonal = c(1,1,2))
summary(arima.eu.pre)
autoplot(acf(test$residuals))
pred.eu <- forecast(arima.eu.pre, h = 8)
diff.pred.eu <- pred.eu$mean - ts.eu[length(ts.eu-7):length(ts.eu)]

#EU plot suggests a gradually increasing intervention effect. This makes sense
#Intuitively 

plot(diff.pred.eu)
  
arima.eu <- auto.arima (ts.eu, seasonal = TRUE, xreg = xreg,
                  allowdrift  = TRUE, allowmean = TRUE)
summary(arima.eu)
autoplot(arima.eu$residuals) + theme_bw()+
  labs(title="European Union: Intervention Analysis Residuals"  , subtitle="SARIMA (3,0,1)(0,1,2)[12]", y="", x="",
       caption=" Y = Forecasted Values - Realized Values")+
  theme(plot.title = element_text( face = "bold", size = "19.5"))+
  theme(plot.subtitle = element_text( face = "italic", size = "12"))+
  theme(axis.text.x =element_text(size=13.5, vjust = -.75)
        , axis.text.y = element_text(size = 13.5 ),
        legend.title=element_text(face = "bold",size=11), 
        legend.text=element_text(size=10, face = "bold"),
        plot.caption = element_text (size = 12))
acf(arima.eu$residuals)

plot(ts.eu)
lines(arima.eu$fitted, type = "l", col = "red")

################################################
#Model fit for Grand Total pre-intervention 

tot <- df[df$country == "GRAND TOTAL",]
tot.pre <- tot[tot$date < "2018-04-01",]
ts.post <- tot[tot$date > "2018-03-01",]
ts.tot <- ts(tot$exports,   frequency = 12)

ts.tot.pre <- ts(tot.pre$exports, frequency = 12)

arima.tot.pre <- auto.arima(ts.tot.pre, trace = TRUE, seasonal = TRUE, allowdrift = TRUE, allowmean = TRUE)
arima.tot.pre1 <- Arima(ts.tot.pre, order = c(2,0,2), seasonal = c(1,1,2),
   include.mean = TRUE, include.drift = TRUE)
summary(arima.tot.pre1)
autoplot(acf(arima.tot.pre1$residuals))
plot(ts.tot.pre)
lines(arima.tot.pre$fitted, type = "l", col = "red")
tot.post.pred <- forecast(arima.tot.pre, h = 8)

#Prediction from pre intervention, suggests that the intervention should start
# in September. 
post <- ts.tot[232:239]
plot(x = seq(1:8), tot.post.pred$mean , type = "l", col = "red")
lines(post, type = "l")

pred.diff.tot <- tot.post.pred$mean - ts.tot[length(ts.tot - 7):length(ts.tot)]

plot(pred.diff.tot)

arima.tot <- auto.arima (ts.tot, seasonal = TRUE, allowdrift = TRUE,
  allowmean =  TRUE,  xreg = xreg)


summary(arima.tot)
plot(ts.tot)
lines(arima.tot$fitted, col = "red", type = "l")

autoplot(arima.tot$residuals) + theme_bw()+
  labs(title="Total Exports: Intervention Analysis Residuals"  , subtitle="SARIMA (3,0,1)(0,1,2)[12]", y="", x="",
       caption=" Y = Forecasted Values - Realized Values")+
  theme(plot.title = element_text( face = "bold", size = "19.5"))+
  theme(plot.subtitle = element_text( face = "italic", size = "12"))+
  theme(axis.text.x =element_text(size=13.5, vjust = -.75)
        , axis.text.y = element_text(size = 13.5 ),
        legend.title=element_text(face = "bold",size=11), 
        legend.text=element_text(size=10, face = "bold"),
        plot.caption = element_text (size = 12))
acf(arima.tot$residuals)

#### Fit intervention model

tot.intervention <- auto.arima(ts.tot, seasonal = TRUE, allowdrift = TRUE,
                               allowmean = TRUE, xreg = xreg)

summary(tot.intervention)
plot(ts.tot)
lines(tot.intervention$fitted, type = "l", col = "blue")

plot(tot.intervention$residuals)


#Some quarterly work 
boxplot(ts.tot~cycle (ts.tot))
ts.qtr <- aggregate(ts.chn, 4)

#ts.tot.qtr <- ts(tot$exports, frequency = 3)
boxplot(ts.qtr~cycle (ts.qtr))

plot(decompose(ts.qtr), type = "multiplicative")

plot(decompose(ts.qtr, type = "multiplicative"))
summary(qtr)



#Some Plots 

#Some Plots 
df1 <- df[!df$country == "GRAND TOTAL",]

p <- ggplot(df1, aes (x = date, y = exports, color = country))+
  geom_line(stat = "identity")+
  theme_bw() + 
labs(title="Monthly Soybean Exports to China and EU", subtitle="Metric Tons", y="", x="",
    caption="Source: Foreign Agriculture Service, Export Query System")+
  theme(plot.title = element_text( face = "bold", size = "19.5"))+
  theme(plot.subtitle = element_text( face = "italic", size = "12"))+
  theme(plot.caption = element_text(size = 13))+
  theme(axis.text.x =element_text(size=13.5, vjust = -.75)
        , axis.text.y = element_text(size = 13.5 ),
        legend.title=element_text(face = "bold",size=11), 
        legend.text=element_text(size=10, face = "bold"))+
   theme(legend.position = c(.15,.75))+
  scale_color_manual(labels = c("China", "EU"), values = c("blue", "red"))+
  labs(color='')
p

as.Date("2002-01-01")
ggplot(tot, aes(x = date, y = exports))+
  geom_line()+
  theme_bw()+
  labs(title="US Total Monthly Soybean Exports", subtitle="Metric Tons", y="", x="",
       caption="Source: Foreign Agriculture Service, Export Query System")+
  theme(plot.title = element_text( face = "bold", size = "19.5"))+
  theme(plot.subtitle = element_text( face = "italic", size = "12"))+
  #theme(plot.caption = element_text(size = 13))+
  theme(axis.text.x =element_text(size=13.5, vjust = -.75)
        , axis.text.y = element_text(size = 13.5 ),
        legend.title=element_text(face = "bold",size=11), 
        legend.text=element_text(size=10, face = "bold"),
        plot.caption = element_text (size = 12))+
  labs(color='Export Destination')

sum(df[df$date > "2016-12-01"& df$date < "2018-01-01",]$exports)/12


sum(df[df$date < "2018-01-01" & df$date > "2016-12-01" & df$country == "CHINA, PEOPLES REPUBLIC OF",]$exports)/

sum(df[df$date < "2018-01-01" & df$date > "2016-12-01" & df$country == "GRAND",]$exports)


