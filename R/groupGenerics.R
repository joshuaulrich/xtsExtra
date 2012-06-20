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

# Not at all committed to this -- 
#   should we use inner joins/merges first? Date alignment like xts? Both? Neither?

Ops.xtsdf <- function(e1, e2 = NULL){
  # Would be much happier to do this without ever using as.df for speed
  as.xtsdf(match.fun(.Generic)(as.data.frame(e1), e2), order.by = index(e1))
}

# This one seems solid
Math.xtsdf <- function(x, ...){
  # Would be much happier to do this without ever using as.df for speed
  .Class <- "data.frame"
  as.xtsdf(NextMethod(.Generic), order.by = index(x))
}

Summary.xtsdf <- function(..., na.rm){
  do.call(match.fun(.Generic), c(lapply(..., as.data.frame), na.rm = na.rm))
}