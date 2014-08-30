#Initialization
historyData <- matrix(nrow = 2048, ncol = 8)
colnames(historyData) <- c("time", "value", "forecast.mean", "lower bounds", "upper bounds", "residuals", "scores", "is_outlier")
historyData[1:500,1]<-testing[1:500,1]
historyData[1:500, 2] <- testing[1:500, 2]
hisCount <- 500
metrics <- testing[501:1000,]

realtimeAnomalyDetect <- function(metrics, step, reportBaseline){
    metricsLength <-nrow(metrics)
    historyts <- ts(historyData[1:hisCount,2])
    metricsts <- ts(metrics[,2])
    
    tmp <- matrix(nrow = metricsLength, ncol = 8)
    tmp[, 1] <- metrics[,1]
    tmp[, 2] <- metrics[,2]
    
    count <- 0
    outliers <- NULL
    while(count < metricsLength){
        metricsforecast <- HoltWinters(historyts, beta = FALSE, gamma = FALSE)
        metricsforecast2 <- forecast.HoltWinters(metricsforecast, h = step, level = 99.5)
        if(count + step <= metricsLength){
            data <- tmp[(count+1):(count + step), 2]
            tmp[(count+1):(count + step), 3] <- metricsforecast2$mean
            tmp[(count+1):(count + step), 4] <- metricsforecast2$lower
            tmp[(count+1):(count + step), 5] <- metricsforecast2$upper
            tmp[(count+1):(count + step), 6] <- abs(metricsforecast2$mean - data)
            outliersTmp <- which(data > metricsforecast2$upper | data < metricsforecast2$lower)
            }
        else{                                          #last few data in metrics
            data <- tmp[(count+1):metricsLength, 2]
            tmp[(count+1):metricsLength, 3] <- metricsforecast2$mean[1:(metricsLength-count)]
            tmp[(count+1):metricsLength, 4] <- metricsforecast2$lower[1:(metricsLength-count)]
            tmp[(count+1):metricsLength, 5] <- metricsforecast2$upper[1:(metricsLength-count)]
            tmp[(count+1):metricsLength, 6] <- abs(metricsforecast2$mean - data)[1:(metricsLength-count)]
            outliersTmp <- which(data > metricsforecast2$upper[metricsLength-count] | data < metricsforecast2$lower[metricsLength-count])
        }
        
        tmp[count + outliersTmp, 8] <- 1
        outliers <- c(outliers, (count + outliersTmp))   #only contains outliers within the single latest input set of data
        
        data[outliersTmp] <- mean(data, trim = 0.2)
        historyts <- c(historyts, data)
    
        count <- count + step
    }
    
    end <- hisCount + metricsLength
    if(end > 2048){
        historyData[1:(2048-metricsLength),] <<- historyData[(hisCount + metricsLength-2047):hisCount,]
        historyData[(2049-metricsLength):2048,] <<- tmp
        hisCount <<- 2048
    }else{
        historyData[(hisCount + 1): end,] <<- tmp
        hisCount <<- hisCount + metricsLength
    }
    
    outliers <- which(historyData[,8] == 1)
    anomalies <- findAnomalies(outliers, reportBaseline)
    
    plot(historyData[,2], type = "l", main = "Anomalies Detection- Real Time")
    lines(historyData[,3], type = "l", col = "red")
    forecastSd <- sd(historyData[, 6], na.rm = TRUE)
    historyData[, 7] <<- historyData[, 6]/forecastSd
    
    blueResidual <- which((historyData[,7] <= 5) & (historyData[,8] == 1))
    yellowResidual <- which((historyData[,7] > 5) & (historyData[,7] <= 9))
    orangeResidual <- which((historyData[,7] > 9) & (historyData[,7] < 12))
    redResidual <- which(historyData[,7] > 12)
    points(blueResidual, historyData[blueResidual, 2], pch = 23, bg = "blue")
    points(yellowResidual, historyData[yellowResidual, 2], pch = 23, bg = "yellow")
    points(orangeResidual, historyData[orangeResidual, 2], pch = 23, bg = "orange")
    points(redResidual, historyData[redResidual, 2], pch = 23, bg = "red")

    points(anomalies, historyData[anomalies, 2], pch = 24, bg = "purple")
    return(list(outliers = historyData[which(historyData[,8] == 1),], anomalies = which(historyData[,8] == 1)[anomalies,]))
}