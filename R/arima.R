arima <- function(x, ...){
  UseMethod("arima")
}

arima.default <- function(x, ...){
  series <- deparse(substitute(x))
  ans <- stats::arima(x, ...)
  ans$series <- series
  ans$call <- match.call()
  
  ans
}

arima.xts <- function(x, ...){
  series <- deparse(substitute(x))
  check.xts.stats(x)
  
  ans <- arima(coredata(x[, 1, drop = FALSE]), ...)
  
  ans$residuals <- xts(ans$residuals, time(x))
  ans$call <- match.call()
  ans$series <- series
  
  class(ans) <- c("xtsArima","Arima")
  
  ans
}

arima0 <- function(x, ...){
  UseMethod("arima0")
}

arima0.default <- function(x, ...){
  series <- deparse(substitute(x))
  
  ans <- stats::arima0(x, ...)
  ans$call <- match.call()
  ans$series <- series
  
  ans
}

arima0.xts <- function(x, ...){
  series <- deparse(substitute(x))
  check.xts.stats(x)
  
  ans <- arima0(coredata(x[, 1, drop = FALSE]), ...)
  
  ans$residuals <- xts(ans$residuals, time(x))
  ans$call <- match.call()
  ans$series <- series
  class(ans) <- c("xtsarima0","arima0")
  
  ans
}
