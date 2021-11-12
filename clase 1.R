

### Clase de series de tiempo 1 ###  https://rpubs.com/tranpaul62/374671

## ESTACIONARIEDAD = MEDIA Y VARIABILIDAD NO CAMBIAN EN EL TIEMPO
## Una serie con media constante es «estacionaria en la media» o «estacionaria»
## Igual con Varianza
## Se da Porque la distribución de Prob es igual en cualquier momento



## RUIDO BLANCO es un ejemplo

r_blanco = rnorm(1000,0,1)

plot.ts(r_blanco, main="Ruido Blanco", xlab="Tiempo", ylab="Valores",col="2")

plot.ts(r_blanco, main="SERIES ESTACIONARIAS", xlab="ESTACIONARIEDAD", ylab="RUIDO BLANCO",col="2")


## No son estacionarias series con Tendencia o estacionalidad


#install.packages("quantmod")
library(quantmod)
#install.packages("tseries")
library(tseries)
#install.packages("fImport")
library(fImport)
#install.packages("fImport")
library(urca)

#Dow Jones Industrial Average (DJIA) es el más importante de todos y
#refleja el comportamiento del precio de la acción de las 30 compañías 
#industriales más importantes y representativas de Estados Unidos 
#https://es.wikipedia.org/wiki/%C3%8Dndice_burs%C3%A1til_Dow_Jones

getSymbols("^DJI",quote="Close",from ="2013-03-17")
DOW= DJI[,4]

plot(DOW,type="l")


## ¿ Qué hacer si necesitamos series estacionarias?


## Modelos econométricos

## estrategias de trading-- Arbitraje Estadístico


## Podemos trandformar las series con diferencias y log

l_dow<-log(DOW)
plot(l_dow,type="l")

r_dow <- na.omit(returns(DOW)) # retorno log
plot(r_dow,type="l")


### test estadísticos para ayudar y para ser mas rápidos
## Los métodos más usados para comprobar si existe estacionariedad
## son los tes de Raiz unitaria



## TEST DE RAICES UNITARIAS

# Si existe Raiz unitaria hay una tendencia estocástica,y por tanto
# no estacionariedad


#getSymbols("^DJI",quote="Close",from ="2002-03-17")
#DOW= DJI[,4]



getFX("EUR/USD") 
plot(EURUSD,type="l")


# Test Dickey Fuller


adf.test(DOW)

help(adf.test)
?adf.test

  
adf.test(EURUSD,k=0)    #en funcion de los datos


#?adf.test


## Hacemos retornos y Comprobamos

r_EURUSD <- na.omit(returns(EURUSD))
plot(r_EURUSD,type="l")

adf.test(r_EURUSD)


#un p-valor de 0.01, indica que rechazamos la hipótesis nula 
#de no estacionariedad.



#trunc((length(EURUSD)-1)^(1/3))


### Urca


## Hay varios tipos de test -- tendencia- constante -nada
?ur.df


df.eurusd <- ur.df(r_dow, type='trend',lags=12, selectlags=c("AIC"))
summary(df.eurusd)


df.eurusd <- ur.df(DOW, type='drift',lags=12, selectlags=c("AIC"))
summary(df.eurusd)

df.eurusd <- ur.df(DOW, type='none',lags=12, selectlags=c("AIC"))
summary(df.eurusd)


## Hacemos los retornos

df.eurusd <- ur.df(r_EURUSD, type='trend',lags=5, selectlags=c("AIC"))
summary(df.eurusd)



# incluir la posibilidad de que el
#término de error no fuera ruido blanco al existir 
#la posibilidad de autocorrelación


# Test Philipps Perron

?pp.test

pp.test(r_EURUSD, alternative="stationary")


## urca

pp <- ur.pp(r_EURUSD, type="Z-tau",model="trend", lags="long")
pp

summary(pp)