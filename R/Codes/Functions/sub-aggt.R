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
