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
