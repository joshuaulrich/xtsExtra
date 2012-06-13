acf <- function(x, ...){
  UseMethod("acf")
}

# Why do we need this? Shouldn't this dispatch to acf.default?
acf.ts <- function(x, ...){
  stats::acf(x, ...)
} 

acf.default <- function(x, ...){
  stats::acf(x, ...)
}

acf.xts <- function(x, ...){
  check.xts.stats(x)
  
  acf(coredata(x[,1, drop = FALSE]), ...)
  
}

pacf.xts <- function(x, lag.max, plot, na.action, ...){
  check.xts.stats(x)
  
  stats::pacf.default(coredata(x[,1, drop = FALSE]), lag.max, plot, na.action, ...)
}

check.xts.stats <- function(x){
  if(!is.regular(x)) warning("Input series is not regular -- treating as such, but results may be unreliable.")
  
  if(NCOL(x) > 1L) warning("Using only the first column.")
}