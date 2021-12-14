#Time series
bf <- Data_Forecasting_of_E_Commerce_Data


library(fpp2)
bf.ts <- ts(bf$`Profit updated` ,start=c(2019,32), end=c(2020,52), frequency=52)
bf.ts
View(bf)
plot(bf.ts)

#Pre covid analysis

#training data

bf.train <- window(bf.ts, end=c(2019,52))
plot(bf.train)  

#testing data

bf.test <- window(bf.ts, start=c(2019,52),end=c(2020,12))
plot(bf.test)


#meann

mean.train<- meanf(bf.train, 13)


attributes(mean.train)


plot(mean.train)


hist(mean.train$residuals)


plot(mean.train)
lines(mean.train$fitted, col="red")
lines(mean.train$residuals, col="blue")
plot(mean.train$fitted,mean.train$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(mean.train$residuals)


accuracy(mean.train,bf.test)

summary(mean.train)

#naive

naive.train<- naive(bf.train, 13)


attributes(naive.train)


plot(naive.train)


hist(naive.train$residuals)


plot(naive.train)
lines(naive.train$fitted, col="red")
lines(naive.train$residuals, col="blue")
plot(naive.train$fitted,naive.train$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(naive.train$residuals)


accuracy(naive.train,bf.test)

summary(naive.train)


#Snaive

snaive.train<- snaive(bf.train, 12)


attributes(snaive.train)


plot(snaive.train)
accuracy(snaive.train)

#rwf

rwf.train<- rwf(bf.train, 13)


attributes(rwf.train)


plot(rwf.train)


hist(rwf.train$residuals)


plot(rwf.train)
lines(rwf.train$fitted, col="red")
lines(rwf.train$residuals, col="blue")
plot(rwf.train$fitted,rwf.train$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(rwf.train$residuals)


accuracy(rwf.train,bf.test)

summary(rwf.train)


#SES
ses.train<- ses(bf.train, 13)


attributes(ses.train)


plot(ses.train)


hist(ses.train$residuals)


plot(ses.train)
lines(ses.train$fitted, col="red")
lines(ses.train$residuals, col="blue")
plot(ses.train$fitted,ses.train$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(ses.train$residuals)


accuracy(ses.train,bf.test)

summary(ses.train)

#ets

ets.train<- ets(bf.train)
ets

attributes(ets.train)


plot(ets.train)


hist(ses.train$residuals)


plot(ses.train)
lines(ses.train$fitted, col="red")
lines(ses.train$residuals, col="blue")
plot(ses.train$fitted,ses.train$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(ses.train$residuals)


accuracy(ets.train)

summary(ses.train)

#ma3,6,12

MA3_forecast <- ma(bf.train,order=3)
MA6_forecast <- ma(bf.train,order=6)
MA12_forecast <- ma(bf.train,order=12)


plot(MA3_forecast, col="Red", main="Graph of Moving average with order 3" )
plot(MA6_forecast, col="Blue", main="Graph of Moving average with order 6")
plot(MA12_forecast, col="Green", main="Graph of Moving average with order 12")


plot(bf.train, main="Graph of Time series graph with Moving avg plot with order 3,6,12")
lines(MA3_forecast, col='Red')
lines(MA6_forecast, col='Blue')
lines(MA12_forecast, col='Green')


#ARIMA

EC_Arima.train <-  auto.arima(bf.train, trace = TRUE, stepwise = FALSE, ic = c("bic"))
EC_Arima.train
plot(EC_Arima.train)
EC_Arima.train <-  auto.arima(bf.train, trace = TRUE, stepwise = FALSE, ic = c("bic"))
EC_Arima.train
EC_Arima_Forecast.train <-  forecast(EC_Arima.train, h = 12)
EC_Arima_Forecast.train
plot(EC_Arima_Forecast.train)
accuracy(EC_Arima_Forecast.train,bf.test)
attributes(EC_Arima_Forecast.train)
EC_Arima_Forecast.train$SSE
EC_Arima_Forecast.train$coefficients




######################################################################################

#pre and post

library(fpp2)
bf.ts <- ts(bf$`Profit updated` ,start=c(2019,32), end=c(2020,52), frequency=52)
bf.ts
View(bf)
plot(bf.ts)



#pre covid

bf.pre<- window(bf.ts, start=c(2019,32), end=c(2020,12))
plot(bf.pre)  

#post covid

bf.post <- window(bf.ts, start=c(2020,12))
plot(bf.post)


#meann

mean.pre<- meanf(bf.pre, 13)


attributes(mean.pre)


plot(mean.pre)


hist(mean.pre$residuals)


plot(mean.pre)
lines(mean.pre$fitted, col="red")
lines(mean.pre$residuals, col="blue")
plot(mean.pre$fitted,mean.pre$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(mean.pre$residuals)


accuracy(mean.pre,bf.post)

summary(mean.pre)

#naive

naive.pre<- naive(bf.pre, 13)


attributes(naive.pre)


plot(naive.pre)


hist(naive.pre$residuals)


plot(naive.pre)
lines(naive.pre$fitted, col="red")
lines(naive.pre$residuals, col="blue")
plot(naive.pre$fitted,naive.pre$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(naive.pre$residuals)


accuracy(naive.pre,bf.post)

summary(naive.pre)


#Snaive

snaive.train<- snaive(bf.train, 12)


attributes(snaive.train)


plot(snaive.train)


#rwf

rwf.train<- rwf(bf.pre, 13)


attributes(rwf.pre)


plot(rwf.pre)


hist(rwf.pre$residuals)


plot(rwf.pre)
lines(rwf.pre$fitted, col="red")
lines(rwf.pre$residuals, col="blue")
plot(rwf.pre$fitted,rwf.pre$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(rwf.pre$residuals)


accuracy(rwf.pre,bf.post)

summary(rwf.pre)


#SES
ses.pre<- ses(bf.pre, 13)


attributes(ses.pre)


plot(ses.pre)


hist(ses.pre$residuals)


plot(ses.pre)
lines(ses.pre$fitted, col="red")
lines(ses.pre$residuals, col="blue")
plot(ses.pre$fitted,ses.pre$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(ses.pre$residuals)


accuracy(ses.pre,bf.post)

summary(ses.pre)

#ets

ets.pre<- ets(bf.pre)
ets

attributes(ets.pre)


plot(ets.pre)


hist(ses.pre$residuals)


plot(ses.pre)
lines(ses.pre$fitted, col="red")
lines(ses.pre$residuals, col="blue")
plot(ses.pre$fitted,ses.pre$residuals,xy.labels = FALSE,xy.lines = FALSE)


Acf(ses.pre$residuals)


accuracy(ets.pre)

summary(ses.pre)

#ma3,6,12

MA3_forecast <- ma(bf.pre,order=3)
MA6_forecast <- ma(bf.pre,order=6)
MA12_forecast <- ma(bf.pre,order=12)


plot(MA3_forecast, col="Red", main="Graph of Moving average with order 3" )
plot(MA6_forecast, col="Blue", main="Graph of Moving average with order 6")
plot(MA12_forecast, col="Green", main="Graph of Moving average with order 12")


plot(bf.pre, main="Graph of Time series graph with Moving avg plot with order 3,6,12")
lines(MA3_forecast, col='Red')
lines(MA6_forecast, col='Blue')
lines(MA12_forecast, col='Green')


#ARIMA

EC_Arima.pre <-  auto.arima(bf.pre, trace = TRUE, stepwise = FALSE, ic = c("bic"))
EC_Arima.pre
plot(EC_Arima.pre)
EC_Arima.pre <-  auto.arima(bf.pre, trace = TRUE, stepwise = FALSE, ic = c("bic"))
EC_Arima.pre
EC_Arima_Forecast.pre <-  forecast(EC_Arima.pre, h = 12)
EC_Arima_Forecast.pre
plot(EC_Arima_Forecast.pre)
accuracy(EC_Arima_Forecast.pre,bf.post)
attributes(EC_Arima_Forecast.pre)
EC_Arima_Forecast.pre$SSE
EC_Arima_Forecast.pre$coefficients






#############################################
library(fpp2)
abc <- lm(bf$`Profit updated` ~lag(bf$`Profit updated`,-1),data = bf)
plot(abc)
