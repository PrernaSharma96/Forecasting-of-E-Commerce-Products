attributes(bf)
class(bf)
bf$Profit

?ts
?frequency

ts1=ts(data=bf$Profit ,start=c(2019,32), end=c(2020,52), frequency=52, deltat = 1/52)
plot(ts1)

Acf(ts1)

ts1

MA5_forecast <- ma(ts1,order=3)
plot(ts1)
lines(MA5_forecast, col='Red')

# To check if the data is seasonal or no

library("fma")
fit <- tbats(ts1)
s <- !is.null(fit$seasonal)
s
ts1


test <- forecast(ts1, h=2)
plot(test)

?stl
