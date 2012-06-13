StructTS <- function(x, ...){
  UseMethod("StructTS")
}

StructTS.default <- function(x, ...){
  stats::StructTS(x, ...)
}

StructTS.xts <- function(x, ...){
  check.xts.stats(x)
  
  ans <- StructTS(coredata(x[, 1, drop = FALSE]), ...)
  
  ans$data <- xts(ans$data, time(x))
  ans$residuals <- xts(ans$residuals, time(x))
  
  class(ans) <- c("xtsStructTS","StructTS")
  
  ans
}

HoltWinters <- function(x, ...){
  UseMethod("HoltWinters")
}

HoltWinters.default <- function(x, ...){
  stats::HoltWinters(x, ...)
}

HoltWinters.xts <- function(x, ...){
  check.xts.stats(x)
  
  ans <- HoltWinters(coredata(x[, 1, drop = FALSE]), ...)
  
  ans$fitted <- as.xts(ans$fitted)
  ans$x <- xts(ans$x, time(x))
  
  class(ans) <- c("xtsHoltWinters","HoltWinters")
  
  ans
}