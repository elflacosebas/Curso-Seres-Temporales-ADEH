
# VISUALIZATION  https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html

library(ggplot2)
library(dplyr)

# Dummy data
data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)

# Most basic bubble plot
p <- ggplot(data, aes(x=day, y=value)) +
  geom_line() + 
  xlab("")
p


p+scale_x_date(date_labels = "%b")
p+scale_x_date(date_labels = "%Y %b %d")
p+scale_x_date(date_labels = "%W")
p+scale_x_date(date_labels = "%m-%Y")


#Breaks and minor breaks
#It also possible to control the amount of break and minor breaks to display with date_breaks and date_minor_breaks.

p + scale_x_date(date_breaks = "1 week", date_labels = "%W")
p + scale_x_date(date_minor_breaks = "2 day")

library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Dummy data
data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) - seq(-140, 224)^2 / 10000
)

# Most basic bubble plot
p <- ggplot(data, aes(x=day, y=value)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

p


#Select time frame
#Use the limit option of the scale_x_date() function to select a time frame in the data:
  
  
  
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Dummy data
data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)

# Most basic bubble plot
p <- ggplot(data, aes(x=day, y=value)) +
  geom_line( color="steelblue") + 
  geom_point() +
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11"))) +
  ylim(0,1.5)

p


####  Loess    http://r-statistics.co/Loess-Regression-With-R.html


?economics


data(economics, package="ggplot2")  # load data
economics$index <- 1:nrow(economics)  # create index variable
economics <- economics[1:80, ]  # retail 80rows for better graphical understanding
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span


smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 



plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")






# define function that returns the SSE
calcSSE <- function(x){
  loessMod <- try(loess(uempmed ~ index, data=economics, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(class(res)!="try-error"){
    if((sum(res, na.rm=T) > 0)){
      sse <- sum(res^2)  
    }
  }else{
    sse <- 99999
  }
  return(sse)
}

# Run optim to find span that gives min SSE, starting at 0.5
optim(par=c(0.5), calcSSE, method="SANN")






#Smoothing with ggplot                       https://ggplot2.tidyverse.org/reference/geom_smooth.html
?mpg

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(orientation = "y")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 0.3)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 0.8) +
  facet_wrap(~drv)














