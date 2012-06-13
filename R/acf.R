acf <- function(x, ...){
  UseMethod("acf")
}

# Why do we need this? Shouldn't this dispatch to acf.default?
acf.ts <- stats::acf 

acf.default <- stats::acf

acf.xts <- function(x, ...){
  if(!is.regular(x)) warning("Input series is not regular -- treating as such but methods may not be reliable.")
  if(NCOL(x) > 1L) warning("Using only the first column.")
  
  acf(coredata(x[,1, drop = FALSE]), ...)
  
}

pacf.xts <- function(x, lag.max, plot, na.action, ...){
  if(!is.regular(x)) warning("Input series is not regular -- treating as such but methods may not be reliable.")
  if(NCOL(x) > 1L) warning("Using only the first column.")
  
  stats::pacf.default(coredata(x[,1, drop = FALSE]), lag.max, plot, na.action, ...)
}
