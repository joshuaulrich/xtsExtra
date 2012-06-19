#   xtsExtra: Extensions to xts during GSOC-2012
#
#   Copyright (C) 2012  Michael Weylandt: michael.weylandt@gmail.com
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

### A first attempt at multi-data-type-xts objects
### For now implemented entirely in R, move to C over time

### Implementation model: 
###   1) List of xts objects, each comprising a single column and a single data type
###   2) Pseudo-inherits to data.frame with a helpful downgrade ?
###   3) Need to handle ... for both xts() and data.frame() -- right now, deferring to data.frame() mostly

xtsdf <- function(..., order.by = index(x), frequency = NULL, unique = TRUE, tzone = Sys.getenv("TZ"), 
                  stringsAsFactors = default.stringsAsFactors(), check.names = TRUE) {
  # xtsdf constructor function
  # uses xts() and data.frame() code instead of rewriting all the name handling
  
  as.xtsdf(data.frame(..., stringsAsFactors = stringsAsFactors, check.names = check.names), 
           order.by = order.by, frequency = frequency, unique = unique, tzone = tzone)
}

as.xtsdf <- function(x, ...) UseMethod("as.xtsdf")

as.xtsdf.xts <- function(x, ...){
  # Easy case -- split by list and add S3 class
  ans <- as.list(x)
  class(ans) <- "xtsdf"
  ans
}

as.xtsdf.data.frame <- function(x, order.by, ..., frequency = NULL, unique = TRUE, tzone = Sys.getenv("TZ")){
  # Next easiest case -- 
  #   Take data frame and order.by argument and construct xts objects directly
  #   Also allow order.by = "rownames" to use x's rownames
  
  if(!is.timeBased(order.by)) {
    if(order.by == "rownames") {
      order.by <- rownames(x)
    }
    order.by <- as.POSIXct(order.by, ...)
  }
  
  ans <- sapply(as.list(d), function(x) xts(x, order.by, frequency = frequency, unique = unique, tzone = tzone))
  class(ans) <- "xtsdf"
  
  ans
}

as.data.frame.xtsdf <- function(x, row.names = NULL, optional = FALSE, ...){
  row.names <- if(is.null(row.names)) index(x) else row.names
  
  do.call("data.frame", list(x, row.names = row.names, check.names = optional, ...))
}

as.xts.xtsdf <- function(x, ...){
  xts(do.call("cbind", x), ...)
}