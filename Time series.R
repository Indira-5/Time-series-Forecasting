library(forecast)

data(AirPassengers)
class(AirPassengers)

start(AirPassengers)

end (AirPassengers)

frequency(AirPassengers)
sum(is.na(AirPassengers))

summary(AirPassengers)
tsdata<- ts(AirPassengers,frequency = 12)

ddata<- decompose(tsdata,"multiplicative")

plot(ddata)

colnames(AirPassengers)
plot(ddata$trend)

plot(ddata$seasonal)

plot(ddata$random)
plot(ddata$figure)
abline(reg=lm(AirPassengers~time (AirPassengers)))
cycle(AirPassengers)

#get boxplot by cycle
boxplot(AirPassengers~cycle(AirPassengers,xlab="Data",ylab="Passenger Numbers (1000's)",main="Monthly Air passengers Boxplot from 1949 to 1961"))

#stationarity 
plot (AirPassengers)

#Ask r for best model
mymodel<- auto.arima(AirPassengers)
mymodel

#lets run with trace to compare the information criter
auto.arima(AirPassengers,ic="aic",trace=TRUE)

#install.packages ("tseries")
library(tseries)
plot.ts(mymodel$residuals)

acf(ts(mymodel$residuals),main='ACF Residual')
pacf(ts(mymodel$residuals),main='PACF Residual')


#use model to forecast for the next 10 years 
myforecast<- forecast(mymodel,level=c(95),h=10*12)
plot(myforecast)

#validate or test the model
Box.test(mymodel$residuals,lag = 5 type="Ljung-Box")
Box.test(mymodel$residuals,lag = 10 type="Ljung-Box")
Box.test(mymodel$residuals,lag = 15 type="Ljung-Box")
