rawData<-read.table("/Users/Elena/Desktop/Auto-Troubleshooting/Real Session Data/mpdb.oracle.Calls.STATE.10s.txt", header=TRUE, sep = "")

# Anomalies detection using time series analysis
detection <- function(metrics = rawData, step = 8, windowSize = 500, reportBaseline = 5) {
    #Create time series for all metrics values
    metrics<-aggt(metrics, 5)
    metricsts <-ts(metrics$VALUE)
    #Count: the number of values already used
    count <- 150
    #windowSize <- 500
    #Initialize dataset for pattern analysis
    forecastData <- metricsts[1:count]
    #Anomalies detected
    outliers <- NULL
    #all the forecasted values: mean
    forecasts <- NULL
    #forecastsResiduals <- NULL
    anomalies <- NULL
    forecastStep <- step
    #the upper and lower forescasts intervals: 80% and 95%
    forecastsUpper <- NULL
    forecastsLower <- NULL
    forecastsUpper2 <- NULL
    forecastsLower2 <- NULL
    forecastsResiduals <- NULL
    
    #-----------------------------------------------------------------------
    # Tranvers the entire time series with step forecastStep
    # method: holt-winters, one dimention, needs to be further optimized
    # Implementation philosophy: each time use the forecastData to predict the values and then compare them with the actual values, and alarts when the actual values fall out of the predicted bounds
    # Change the anomalies to the appropriate values for the future predictions' accuracy
    # and add all the values to the forecastData after comparasion
    #-----------------------------------------------------------------------
    
    while(count<length(metricsts)){
        if(length(forecastData) > windowSize){
           forecastData <- forecastData[201: length(forecastData)]
        }
        
        #KEY:forecast...
        metricsforecast<-HoltWinters(forecastData, beta=FALSE, gamma=FALSE)
        metricsforecast2<-forecast.HoltWinters(metricsforecast, h = forecastStep, level = c(95, 99.5))
        
        #arima <- auto.arima(forecastData)
        #metricsforecast2 <- forecast.Arima(arima, h = forecastStep, level = c(95, 99.5))
        forecasts <- c(forecasts, metricsforecast2$mean)
        forecastsResiduals <- c(forecastsResiduals, metricsforecast2$residuals)
        
        forecastsUpper <- c(forecastsUpper, metricsforecast2$upper[, 2])
        forecastsLower <- c(forecastsLower, metricsforecast2$lower[, 2])
        forecastsUpper2 <- c(forecastsUpper2, metricsforecast2$upper[, 1])
        forecastsLower2 <- c(forecastsLower2, metricsforecast2$lower[, 1])

        data <- metricsts[(count+1):(count+forecastStep)]
        #printOutliers(data, metricsforecast2$upper[, 2], metricsforecast2$lower[, 2])
        tmp <- which(data > metricsforecast2$upper[, 2] | data < metricsforecast2$lower[, 2])
        
        #异常的检测机制仅仅是靠预测，那么万一预测不准怎么办。。加入另一个种机制相结合！
        outliers <- c(outliers,(count + tmp))
        #outliers adjustment，这里是不是可以改的合理一点
        data[tmp] <- mean(data, trim = 0.4)
        forecastData <- c(forecastData, data)
        count <- count + forecastStep
    }
    
    anomalies <- findAnomalies(outliers, reportBaseline)
    anomalies[,1] <- outliers[anomalies[,1]]

    #begin plotting
    plot(metricsts, type = "l", main = "Anomalies Detection")
    lines(151:(150+length(forecasts)), forecasts, type = "l", col = "red")
    #lines(151:(150+length(forecastsUpper)), forecastsUpper, type = "l", lty = "dashed", col = "blue")
    #lines(151:(150+length(forecastsLower)), forecastsLower, type = "l", lty = "dashed", col = "blue")
    #lines(151:(150+length(forecastsUpper2)), forecastsUpper2, type = "l", lty = "dashed", col = "gold")
    #lines(151:(150+length(forecastsLower2)), forecastsLower2, type = "l", lty = "dashed", col = "gold")
    points(anomalies[,1], metricsts[anomalies[,1]], pch = 24, bg = "red")
    
    #Compute outliers' residuals and scores
    
    #这里计算的是整个序列的残差标准差，效率极其不高！！！且没办法实现实时计算
    forecastSd <- sd(forecastsResiduals)
    outliersResiduals <- forecasts[outliers-150]-metricsts[outliers]
    outliersScores <- abs(outliersResiduals/forecastSd)
    
    #Categorize outliers into four degrees according to their severity and mark them with #different symbols
    blueResidual <- which(outliersScores <= 5)
    yellowResidual <- which((outliersScores > 5) & (outliersScores <= 9))
    orangeResidual <- which((outliersScores > 9) & (outliersScores < 12))
    redResidual <- which(outliersScores > 12)
    points(outliers[blueResidual], metricsts[outliers[blueResidual]], pch = 23, bg = "blue")
    points(outliers[yellowResidual], metricsts[outliers[yellowResidual]], pch = 23, bg = "yellow")
    points(outliers[orangeResidual], metricsts[outliers[orangeResidual]], pch = 23, bg = "orange")
    points(outliers[redResidual], metricsts[outliers[redResidual]], pch = 23, bg = "red")
    
    results1 <- cbind(metrics[outliers,], ForecastValues = forecasts[outliers-150], Residuals= outliersResiduals, Scores = outliersScores)
    results2 <-anomalies
    
    return(list(outliers = results1, anomalies = results2))
}

findAnomalies <- function(outliers, k){
    results <- NULL
    if(length(outliers) != 0){
        difference <- outliers[2:length(outliers)] - outliers[1:(length(outliers)-1)]
        anomalies <- NULL
        aCount <- NULL
        j <- 1
        while(j < length(difference) - k + 3){
            if(all(difference[j : (j + k - 2)] == 1)){
                anomalies <- c(anomalies, j)
                aCount[length(anomalies)] <- k
                j <- j+ k - 1
                while(j < (length(difference) + 1)){
                    if(difference[j] != 1){
                        j <- j + 1
                        break
                    }
                    aCount[length(anomalies)] <- aCount[length(anomalies)]+1
                    j <- j + 1
                }
            }else j <- j + 1
        }
        results <- cbind(Index = anomalies, Count = aCount)

    }
    return(results)
}

multiMetricsDetection(metricsList){
    results <- NULL
    for(n in metricsList){
        singleResult <- detection(metrics = n，step = 15)
        results  <- list(singleResult, )
    }
}

#Anomalies detection using median absolute deviation
madDetection<- function(){
    windowSize <- 535
    madBaseLinePercentile <- 0.9
    metricsts <- ts(metrics$VALUE)
    stats <- metricsts[1: windowSize]
    madStats <- c()
    dm <- c()
    outliers <- c()
    
    # Compute Median absolute deviation
    madBase <- quantile(stats, prob = c(madBaseLinePercentile))
    for(element in stats){
        deviation <- abs(element - madBase)
        madStats <- c(madStats, deviation)
    }
    mad <- median(madStats)
    
    #Compute dm = d(t)/mad and dmMax
    for(i in 1:length(stats)){
        dm <- c((madStats[i]/mad), dm)
    }
    dmMax = quantile(dm, c(0.95))
    
    #detect anomelies
    outliers <- c(outliers,which(dm > dmMax & stats < madBase))
    
    outliers
}

aggt <- function(ts, step){
    t <- ts[[1]]
    v <- as.numeric(ts[[2]])
    i <- 1
    METRICS_TIME<- NULL
    VALUE <- NULL
    while(i < nrow(ts)){
        VALUE <- c(VALUE, mean(v[i:(i+step-1)], na.rm = TRUE))
        METRICS_TIME <- c(METRICS_TIME, floor(median(t[i:(i+step-1)], na.rm = TRUE)))
        i <- i+step
    }
    return(data.frame(METRICS_TIME, VALUE, stringsAsFactors = FALSE))
}
