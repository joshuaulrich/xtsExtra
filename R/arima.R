arima <- function(x, ...){
  UseMethod("arima")
}

arima.default <- function(x, ...){
  stats::arima(x, ...)
}

arima.xts <- function(x, ...){
  check.xts.stats(x)
  
  ans <- arima(x[, 1, drop = FALSE], ...)
  
  ans$residuals <- as.xts(ans$residuals, time(x))
  
  class(ans) <- c("xtsArima","Arima")
  
  ans
}

arima0 <- function(x, ...){
  UseMethod("arima0")
}

arima0.default <- function(x, ...){
  stats::arima(x, ...)
}

arima0.xts <- function(x, ...){
  check.xts.stats(x)
  
  ans <- arima0(x[, 1, drop = FALSE], ...)
  
  ans$residuals <- as.xts(ans$residuals, time(x))
  
  class(ans) <- c("xtsarima0","arima0")
  
  ans
}
