

library(data.table)
#https://www.ine.es/jaxiT3/Datos.htm?t=35177#!tabs-tabla

setwd("C:/Users/Y3923679P/Desktop/CURSO ADEH 2021")

myts <- fread('35177bsc2.csv',header=T, sep=';', data.table=F)

myts2 <-  ts(myts$Total, frequency=52)

plot(myts2)

adf.test(myts2)

acf(myts2)
pacf(myts2)


fit <- auto.arima(myts$Total)    

forecast(fit, 5)
plot(forecast(fit, 50))
