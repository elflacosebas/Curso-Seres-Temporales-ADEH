
### ejemplo

#install.packages("astsa")
library(help="astsa")
library(astsa)

austres

plot(austres, xlab="Year",ylab="Number of Australian Residents", main="Australian Population in thousand")

austres[1] # show the first element
length(austres) # show the number of elements
dim(austres) # no dimensions..

d_austres <- diff(log(austres)) # difference 
plot(d_austres) # plot it

shapiro.test(d_austres) # perform test to show normality of data

plot(as.numeric(austres),lag(as.numeric(austres))) # starts one year earlier.
lag1.plot(d_austres,4) #produce a grid of scatterplots of a series versus lagged values of the series

Q <- factor(cycle(austres)) # split into quarter factors
trend <- time(austres) - 1980 # center the time to make the results look better using 1980 as midway in the whole dataset
reg <- lm(log(austres) ~ 0 + trend + Q, na.action=NULL) # run the regression without intercept
summary(reg)















