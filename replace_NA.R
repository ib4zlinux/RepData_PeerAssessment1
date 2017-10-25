replace_NA <- function(data) {
        
        
        obs <- nrow(data)
        n <- 1
 
        while (n <= obs) {     
                if (is.na(data$steps[n])) {
                        interval <- data$interval[n];
                        x <- which(steps_by_interval$interval == interval);
                        data$steps[n] <- steps_by_interval$steps[x]
                
                }
        
                n <- n + 1
        }
        assign('data',data,envir=.GlobalEnv)
}