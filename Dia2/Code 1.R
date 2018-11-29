# Descriptive Statistics

library(ggplot2)
x=rnorm(1000,0,1)
sd(x)
summary(x)
hist(x)
theme(plot.title = element_text(hjust = 0.5))
qplot(x, geom="histogram",binwidth = 0.3,col=I("red"),main="Histogram of X")
boxplot(x,col="red",main="Boxplot of X")
x[43]=10
boxplot(x,col="red",main="Boxplot of X")

# Reading Builtin Datasets

AirPassengers
data(AirPassengers)
ts.plot(AirPassengers)

#Reading External Files

beer<-read.table("/Users/ronnyvallejos/Documents/beer.csv")

# Creating Images


x=matrix(rnorm(100,0,1),ncol=10, nrow=10)
par(mfrow=c(1,2), pty="s",oma=c(0.2,0.2,0.2,0.2))
image(x)
image(x, col  = gray((0:32)/32))
