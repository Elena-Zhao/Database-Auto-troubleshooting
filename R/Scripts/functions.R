metrics<-read.table("/Users/Elena/Desktop/Auto-Troubleshooting/R/DATA/db.biz.items.bids.txt", header=TRUE, sep = "")

# Anomalies detection using time series analysis
detection <- function(metrics) {
    #Create time series for all metrics values
    metricsts <-ts(metrics$VALUE)
    #Count: the number of values already used
    count <- 150
    windowSize <- 500
    #Initialize dataset for pattern analysis
    forecastData <- metricsts[1:count]
    #Anomalies detected
    outliers <- NULL
    #all the forecasted values: mean
    forecasts <- NULL
    anomalies <- NULL
    forecastStep <- 2
    #the upper and lower forescasts intervals: 80% and 95%
    forecastsUpper <- NULL
    forecastsLower <- NULL
    forecastsUpper2 <- NULL
    forecastsLower2 <- NULL
    
    #-----------------------------------------------------------------------
    # Tranvers the entire time series with step forecastStep
    # method: holt-winters, one dimention, needs to be further optimized
    # Implementation philosophy: each time use the forecastData to predict the values and then compare them with the actual values, and alarts when the actual values fall out of the predicted bounds
    # Change the anomalies to the appropriate values for the future predictions' accuracy
    # and add all the values to the forecastData after comparasion
    #-----------------------------------------------------------------------
    
    while(count<length(metricsts)){
        #if(length(forecastData) > windowSize){
        #   forecastData <- forecastData[201: length(forecastData)]
        #}
        metricsforecast<-HoltWinters(forecastData, beta=FALSE, gamma=FALSE)
        #metricsforecast<-HoltWinters(forecastData)
        metricsforecast2<-forecast.HoltWinters(metricsforecast, h = forecastStep)
        forecasts <- c(forecasts, metricsforecast2$mean)
        forecastsUpper <- c(forecastsUpper, metricsforecast2$upper[, 2])
        forecastsLower <- c(forecastsLower, metricsforecast2$lower[, 2])
        forecastsUpper2 <- c(forecastsUpper2, metricsforecast2$upper[, 1])
        forecastsLower2 <- c(forecastsLower2, metricsforecast2$lower[, 1])

        data <- metricsts[(count+1):(count+forecastStep)]
        #printOutliers(data, metricsforecast2$upper[, 2], metricsforecast2$lower[, 2])
        tmp <- which(data > metricsforecast2$upper[, 2] | data < metricsforecast2$lower[, 2])
        outliers <- c(outliers,(count + tmp))
        
        #outliers adjustment
        #data[tmp] <- metricsforecast2$mean[tmp]
        
        forecastData<-c(forecastData, data)
        count <- count + forecastStep
    }
    
    anomalies <- findAnomalies(outliers, 3)
    
    #begin plotting
    
    plot(metricsts, type = "l", main = "Anomalies Detection")
    lines(151:(150+length(forecasts)), forecasts, type = "l", col = "red")
    lines(151:(150+length(forecastsUpper)), forecastsUpper, type = "l", lty = "dashed", col = "blue")
    lines(151:(150+length(forecastsLower)), forecastsLower, type = "l", lty = "dashed", col = "blue")
    #lines(151:(150+length(forecastsUpper2)), forecastsUpper2, type = "l", lty = "dashed", col = "gold")
    #lines(151:(150+length(forecastsLower2)), forecastsLower2, type = "l", lty = "dashed", col = "gold")
    points(outliers, metricsts[outliers], pch = 23, bg = "yellow")
    points(outliers[anomalies[,1]], metricsts[anomalies[,1]], pch = 24, bg = "red")
    
    results1 <- cbind(metrics[outliers,], ForecastValues = forecasts[outliers-150], Residuals= (forecasts[outliers-150]-metricsts[outliers]))
    results2 <-anomalies
    return(list(outliers = results1, anomalies = results2))
}

findAnomalies <- function(outliers, k){
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
    return(results)
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