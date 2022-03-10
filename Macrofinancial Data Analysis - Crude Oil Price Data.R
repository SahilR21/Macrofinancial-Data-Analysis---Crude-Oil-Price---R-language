#CHAPTER NO. 04 - MACROFINANCIAL DATA ANALYSIS
## Macroeconomics - how whole economy works

#Project - 1 - Crude oil price data from 1987 to 2017
##installing new library and load into R environment
###install.packages("fBasics")
###install.packages("evir")
###install.packages("qrmdata")
library(fBasics)
library(evir)
library(qrmdata)
library(zoo)

##load the data
crudedata = read.csv("CrudeOilPriceData.csv")
head(crudedata)
str(crudedata)
##clean the data
crudedata_1 = na.omit(crudedata)
head(crudedata_1)
## compute the rates of change for brent oil price next
brent.return = as.zoo(na.omit((diff(log(crudedata$Value)))))[-1]*100
colnames(brent.return) = "Brent.return"
head(brent.return)

##Let's look at the box plt will show the minimum to maximum with mean in the middle of box
png("Brent Daily % change - 1.png")
boxplot(as.vector(brent.return),title = FALSE,
        main = "Brent Daily % change", col = "blue",cex = 0.5, pch = 19)
dev.off()
skewness(brent.return)
kurtosis(brent.return)

## We want to look at likelihood of positive versus negative returns. We might want to review skewness and kurtosis defination adn range to help us
png("Brent daily autocorrelogram - 2.png")
acf(coredata(brent.return), main = "Brent daily autocorrelogram",
    lag.max = 20, ylab = "",xlab = "",col = "blue",ci.col = "red")
dev.off()
### Confidence intervals are the red dashed lines.  
### ACF at lag 6 means the correlation of current Brent returns with 
### returns 6 trading days ago, including any correlations from trading day 1through 6.

png("Brent daily partial autocorrelogram - 3.png")
pacf(coredata(brent.return),main = "Brent daily partial autocorrelogram",
     lag.max = 20, ylab = "",xlab = "", col = "blue",ci.col = "red")
dev.off()
### PACF is simpler: it is the raw correlation between day 0 and day 6. ACF starts
### at lag 0 (today); PACF starts at lag 1 (yesterday).

##How many trading days in a typical week or in a month? and how thick is that tail?
png("Brent Daily Returns - 4.png")
boxplot(as.vector(brent.return), title = FALSE,
        main = "Brent Daily Returns", col = "blue",
        cex = 0.5, pch = 10)
dev.off()

##Share of graph distribution
skewness(brent.return)
###Negative skewness means data distribution is left side and have long tail on left side

kurtosis(brent.return)
###High kurtosis means the high and sharp peak

##Something Interesting - acf() to explore autocorrelation
png("Brent Autocorrelogram - 5.png")
acf(coredata(brent.return), main = "Brent Autocorrelogram",lag.max = 20, ylab = "",xlab = "", col = "blue",ci.col = "red")
dev.off()

png("Brent Partial Autocorrelogram - 6.png")
pacf(coredata(brent.return), main = "Brent Partial Autocorrelogram", lag.max = 20, ylab = "", xlab = "",
     col = "blue", ci.col = "red" )
dev.off()
### it shows that - there are some postitive weekly and monthly negative cycle

## Size of brent return - absolute valeu of return (  think of oil and countries entering and leaving EU) can signal contagion, herf mentality and simply veru large magin call.
brent.return.abs = abs(brent.return)
### trading position size matters
brent.return.tail = tail(brent.return.abs[order(brent.return.abs)], 100)[1]
brent.return.tail
### take just first of 10 observatipn and pick the first
### build an index of these sizes that exceed the heavy tail threshhold
#brent.return.abs.tail = timeSeries(rep(0,length(brent.return)), charvec = time(brent.return))
### just a lot of zeros we wi fill up - next
#brent.return.abs.tail[index,1] = brent.return.abs[index]
###A phew is in order

## plot
png("Brent daily return size - 7.png")
plot(brent.return.abs, xlab = "", main = "Brent Daily Return Sizes",
     col = "blue")
dev.off()
### it show lot of volatility  - just in oute size along

## Brent autocorrelogram - give correlation
png("Brent autocorrelogram - 8.png")
acf(coredata(brent.return.abs), main = "Brent Autocorrelogram",
    lag.max = 60, ylab = "", xlab = "",
    col = "blue", ci.col = "red")
dev.off()

##Brnet autocorrelogram
png("brent autocorrelogram - 9.png")
acf(coredata(brent.return.abs), main = "Brent Autocorrelogram",
    lag.max = 60, ylab = "", xlab = "",
    col = "blue", ci.col = "red")
dev.off()

### it show that there is volatility clustering with strong persistent lags of absolute movements in return evidence by ACF plot. There is evidene of dampening with after 
#shocks past traidng 10 day 10 ago. Monthly volatility effect today's performance.





