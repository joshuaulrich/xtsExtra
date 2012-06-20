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

`[.xtsdf` <- function(x, i, j, drop = FALSE, which.i = FALSE, ...){
  if(missing(i)) {
    x <- unclass(x)[j]
    class(x) <- "xtsdf"
    return(x)
  }
     
  x <- x[,j]
     
  # For now simply loop over x and use i as appropriate
  ans <- lapply(x, function(x) x[i = i, drop = drop, which.i = which.i, ...])   
  class(ans) <- "xtsdf"
  
  ans
  
}

`[<-.xtsdf` <- function(x, i, j, value){
  # Would be much happier to do this without ever using as.df for speed
  x.df <- as.data.frame(x, stringsAsFactors = FALSE)
  x.df[i,j] <- value
  as.xtsdf(x.df, order.by = index(x))
}

print.xtsdf <- function(x, ...){
  # Would be much happier to do this without ever using as.df for speed
  print(as.data.frame(x, row.names = index(x)), ...)
}

str.xtsdf <- function(object, ...) {
  cat(paste("An", sQuote("xtsdf"), "object from", index(first(object[[1]])), 
            "to", index(last(object[[1]])),"containing",NROW(object),"observations of",NCOL(object),"variables:\n\n"))
  
  for(i in seq_len(NCOL(object))){
    # Should align names more attractively? 
    
    cat(" ", names(object)[i],"$: ")
    str(coredata(object[[i]]))
  }
  cat("\n")
  cat(paste("  Indexed by objects of class: "))
  cat(paste("[", paste(indexClass(object), collapse = ","), 
            "] ", sep = ""))
  cat(paste("TZ: ", indexTZ(object), "\n", sep = ""))
  if (!is.null(CLASS(object))) 
    cat(paste("  Original class: '", CLASS(object), "' ", 
              sep = ""), "\n")
}
