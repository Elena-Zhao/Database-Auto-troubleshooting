sink("/Users/Elena/Desktop/R/Reports/db.os.memory.kernel1", append=TRUE, split=TRUE)

library(forecast)
library(TSA)
metrics<-read.table("/Users/Elena/Desktop/R/DATA/db.os.memory.kernel1.txt", header=TRUE, sep = "");
metricsts<-ts(metrics$VALUE, frequency=90)

logmetricsts<-log(metricsts)
#plot.ts(logmetricsts)

metricscomponents<-decompose(metricsts)
metricsseasonallyadjustd<-metricsts-metricscomponents$seasonal
summary(metricscomponents)

metricsforecast<-HoltWinters(metricsts)
summary(metricsforecast)
metricsforecast$SSE
#plot(metricsforecast)

metricsforecast2<-forecast.HoltWinters(metricsforecast, h=50)
#plot.forecast(metricsforecast2)
summary(metricsforecast2)

#acf(metricsforecast2$residuals, lag.max = 20)
Box.test(metricsforecast2$residuals, lag = 20, type="Ljung-Box")
#dev.new()
#plot.ts(metricsforecast$residuals)

pdf("/Users/Elena/Desktop/R/Reports/db.os.memory.kernel1.pdf")
	plot.ts(metricsts)
	title(main="metrics time series", sub="Fig.1")
	plot.ts(logmetricsts)
	title(main="metrics time series log", sub="Fig.2")
	plot(metricscomponents)
	title(main="metrics decomposition", sub="Fig.3")
	plot(metricsseasonallyadjustd)
	title(main="metrics seasonally adjusted", sub="Fig.4")
	plot(metricsforecast)
	title(sub="Fig.5")
	plot.forecast(metricsforecast2)
	title(sub="Fig.6")
	acf(metricsforecast2$residuals, lag.max = 20)
	title(sub="Fig.7")
	plot.ts(metricsforecast2$residuals)
	title(main="metrics forecast2 residuals", sub="Fig.8")
	#plotForecastErrors(metricsforecast2$residuals)
	plotForecastErrors(metricsforecast2$residuals)
dev.off()
