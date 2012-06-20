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

stl<- function(x, ...){
  UseMethod("stl")
}

stl.default <- function(x, ...){
  ans <- stats::stl(x, ...)
  ans[["call"]] <- match.call()
  
  ans
}

stl.xts <- function(x, ...){
  check.xts.stats(x)
  
  ans <- stl(coredata(x[, 1, drop = FALSE]), ...)
  
  ans[["time.series"]] <- xts(ans[["time.series"]], time(x))
  ans[["call"]] <- match.call()
  
  class(ans) <- c("xtsStl","stl")
  
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