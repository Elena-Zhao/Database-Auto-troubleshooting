sink("/Users/Elena/Desktop/R/Reports/db.biz.items.bids", append=TRUE, split=TRUE)

library(forecast)
library(TSA)
metrics<-read.table("/Users/Elena/Desktop/R/DATA/db.biz.items.bids.txt", header=TRUE, sep = "");
metricsts<-ts(metrics$VALUE)
cat("The summary of metrics time series:\n")
summary(metricsts)

metricsforecast<-HoltWinters(metricsts, beta=FALSE, gamma=FALSE)
cat("The summary of metrics forecast using HoltWinters:\n")
summary(metricsforecast)

cat("metricsforecast$SSE:\n")
summary(metricsforecast$SSE)

metricsforecast2<-forecast.HoltWinters(metricsforecast, h=8)
summary(metricsforecast2)

#boxtest1<-Box.test(metricsforecast$residuals, lag = 20, type="Ljung-Box")
boxtest2<-Box.test(metricsforecast2$residuals, lag = 20, type="Ljung-Box")
summary(boxtest2)

plotForecastErrors <- function(forecasterrors) {
# make a red histogram of the forecast errors: 
mybinsize <- IQR(forecasterrors)/4mysd <- sd(forecasterrors)mymin <- min(forecasterrors) + mysd*5mymax <- max(forecasterrors) + mysd*3mybins <- seq(mymin, mymax, mybinsize) 
hist(forecasterrors, col="red", freq=FALSE, breaks=100) 
# freq=FALSE ensures the area under the histogram = 1
# generate normally distributed data with mean 0 and standard deviation mysd 
mynorm <- rnorm(10000, mean=0, sd=mysd)myhist <- hist(mynorm, plot=FALSE, breaks=9)# plot the normal curve as a blue line on top of the histogram of forecast errors: 
points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)}

pdf("/Users/Elena/Desktop/R/Reports/db.biz.items.bids.pdf")
	plot.ts(metricsts)
	#plot(metricsforecast)
	plot.forecast(metricsforecast2)
	#acf(metricsforecast$residuals, lag.max = 20)
	acf(metricsforecast2$residuals, lag.max = 20)
	#plot.ts(metricsforecast2$residuals)
	plotForecastErrors(metricsforecast2$residuals)
dev.off()

sink()
