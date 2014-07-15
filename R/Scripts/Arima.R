sink("/Users/Elena/Desktop/R/Reports/db.biz.items.listing-arima", append=TRUE, split=TRUE)

library(forecast)
library(TSA)
metrics<-read.table("/Users/Elena/Desktop/R/DATA/db.biz.items.listing.txt", header=TRUE, sep = "");
#metrics
metricsts<-ts(metrics$VALUE)
#metricsts
#dev.new()
#plot.ts(metricsts)
# > metricsdiff1<-diff(metricsts, differencs=1)
# > plot(metricsdiff1)
metricsarima<-auto.arima(metricsts)
summary(metricsarima)
metricsforecast<-forecast.Arima(metricsarima, h=20)
summary(metricsforecast)
#dev.new()
#plot.forecast(metricsforecast)
#dev.new()
acf(metricsforecast$residuals, lag.max = 20)
boxtest<-Box.test(metricsforecast$residuals, lag = 20, type="Ljung-Box")
summary(boxtest)
#dev.new()
#plot.ts(metricsforecast$residuals)

pdf("/Users/Elena/Desktop/R/Reports/db.biz.items.listing-arima.pdf")
	plot.ts(metricsts)
	plot.forecast(metricsforecast)
	acf(metricsforecast$residuals, lag.max = 20)
	plot.ts(metricsforecast$residuals)
    plotForecastErrors(metricsforecast$residuals)

dev.off()