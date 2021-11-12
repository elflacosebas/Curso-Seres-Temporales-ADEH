
library(datasets)


sunspot.month 

#Monthly numbers of sunspots, as from the World Data Center, aka SIDC. 
#This is the version of the data that will occasionally 
#updated when new counts become available.

https://www.statmethods.net/advstats/timeseries.html


myts <- ts(sunspot.month, frequency=12)

# subset the time series (June 2014 to December 2014)
myts2 <- window(myts)

# plot series
plot(myts)

# Seasonal decomposition
fit <- stl(myts, s.window="period")
plot(fit)

# additional plots
monthplot(myts)
library(forecast)
seasonplot(myts)


############Exponential Models

#El suavizado exponencial es un método de pronóstico de series de tiempo para datos univariados.
#Los métodos de series de tiempo como la familia de métodos ARIMA de Box-Jenkins desarrollan un modelo en el que la predicción es una suma lineal ponderada de observaciones pasadas recientes o retrasos.
#Los métodos de pronóstico de suavizado exponencial son similares en que una predicción es una suma ponderada de observaciones pasadas, pero el modelo usa explícitamente una ponderación exponencialmente decreciente para las observaciones pasadas.

#####Both the HoltWinters() function in the base installation, and the ets() function in the forecast package, can be used to fit exponential models.

# simple exponential - models level
fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(myts, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(myts)

# predictive accuracy
library(forecast)


# predict next three future values
library(forecast)
forecast(fit, 50)
plot(forecast(fit, 50))


# fit an ARIMA model of order P, D, Q
fit <- arima(myts, order=c(p, d, q))
             
             # predictive accuracy
             library(forecast)
             accuracy(fit)
             
             # predict next 5 observations
             library(forecast)
             forecast(fit, 5)
             plot(forecast(fit, 5))


             
acf(myts)   #https://support.minitab.com/es-mx/minitab/18/help-and-how-to/modeling-statistics/time-series/how-to/autocorrelation/interpret-the-results/autocorrelation-function-acf/#:~:text=La%20funci%C3%B3n%20de%20autocorrelaci%C3%B3n%20es,t%20e%20y%20t%E2%80%93k). 
pacf(myts)    #https://support.minitab.com/es-mx/minitab/18/help-and-how-to/modeling-statistics/time-series/how-to/partial-autocorrelation/interpret-the-results/partial-autocorrelation-function-pacf/
             
             library(forecast)
             # Automated forecasting using an exponential model
             fit <- ets(myts)
             
             # Automated forecasting using an ARIMA model
             fit <- auto.arima(myts)             
             
             

             
             
 ### cross correlation function
             
             Spend <- c(5, 3, 6, 5, 8, 9, 10, 17, 12, 11, 10, 9)
             Income <- c(25, 29, 22, 34, 22, 28, 29, 31, 34, 45, 45, 40)
             ccf(Spend, Income, na.action=na.omit)
             
             x <- rnorm(10)
             y <- -lead(x)
             ccf(x, y, na.action=na.omit)
             
             
                        
             
             
             
             
            


