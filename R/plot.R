#
#   xtsExtra: Extensions to xts during GSOC-2012
#
#   Copyright (C) 2012  Michael Weylandt: michael.weylandt@gmail.com
#
#   Scatterplot code taken from plot.zoo in the CRAN zoo package
#   
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

# SHOULD REMOVE par() ARGS FROM FORMALS AND INSTEAD TREAT ... BETTER
`plot.xts` <- function(x, y = NULL, 
                       screens, screens.layout,
                       type, auto.grid=TRUE,
                       major.ticks='auto', minor.ticks=TRUE, 
                       major.format=TRUE,
                       bar.col='grey', candle.col='white',
                       ann, axes, xlab, ylab, main, xlim, ylim,
                       xy.labels = FALSE, xy.lines = NULL, 
                       ...) {
  
  ## if y supplied: scatter plot y ~ x
  if(!is.null(y)) {
    if(NCOL(x) > 1 || NCOL(y) > 1) stop("Scatter plots only for univariate series")
    
    # Catch these early enough?
    xlab <- if(missing(xlab)) deparse(substitute(x)) else xlab
    ylab <- if(missing(ylab)) deparse(substitute(y)) else ylab
    
    x <- try.xts(x); y <- try.xts(y)
    
    xy.xts <- merge(x, y, join = "inner")
    
    xy <- coredata(xy.xts)
    
    xy <- xy.coords(xy[,1], xy[,2])

    xlim <- if(missing(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
    ylim <- if(missing(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
    
    if(missing(main)) main <- paste(xlab, "vs.", ylab)

    do.lab <- if(is.logical(xy.labels)) xy.labels else TRUE
    
    if(is.null(xy.lines)) xy.lines <- do.lab
    
    ptype <- if(do.lab) "n" else if(missing(type)) "p" else type

    plot.default(xy, type = ptype, main = main, xlab = xlab, 
      ylab = ylab, xlim = xlim, ylim = ylim, ...)
      
    if(do.lab) text(xy, 
      labels = if(!is.logical(xy.labels)) xy.labels else index2char(index(xy.xts)), ...)
    if(xy.lines) lines(xy, type = if(do.lab) "c" else "l", ...) 

    return(invisible(xy.xts))
  }
  
  ## Else : no y, only x
  x <- try.xts(x)
  
  # By default one screen per panel
  if(missing(screens)){
    screens <- 1:NCOL(x)
  }

  if(missing(screens.layout)){
    screens.layout <- seq_along(unique(screens))
  }
  
  #####
  #
  #  SOME CODE TO MAKE SURE screens.layout IS LEGAL
  #
  #####
  
  
  #time.scale <- periodicity(x)$scale
  ep <- axTicksByTime(x,major.ticks, format.labels=major.format)

  otype <- type

  if(xts:::is.OHLC(x) && type %in% c('candles','bars')) {
    x <- x[,xts:::has.OHLC(x, TRUE)]
    xycoords <- list(x=.index(x), y=seq(min(x),max(x),length.out=NROW(x)))
    type <- "n"
  } else {
    if(NCOL(x) > 1) warning('only the univariate series will be plotted')
    if(is.null(y))
      xycoords <- xy.coords(.index(x), x[,1])
  }

  plot(xycoords$x, xycoords$y, type=type, axes=FALSE, ann=FALSE, ...)

  if(auto.grid) {
    abline(v=xycoords$x[ep], col='grey', lty=4)
    grid(NA,NULL)
  }

  if(xts:::is.OHLC(x) && otype == 'candles')
    xts:::plot.ohlc.candles(x, bar.col=bar.col, candle.col=candle.col, ...)

  dots <- list(...)

#  if('axes' %in% names(dots)) {
#    if(!dots$axes) axes <- FALSE
#  } else axes <- TRUE

  if(axes) {
    if(minor.ticks)
      axis(1, at=xycoords$x, labels=FALSE, col='#BBBBBB', ...)
    axis(1, at=xycoords$x[ep], labels=names(ep), las=1, lwd=1, mgp=c(3,2,0),...) 
    axis(2, ...)
  }
  
  box()

  if(!'main' %in% names(dots)) title(main)
  do.call('title',list(...))
  assign(".plot.xts",recordPlot(),.GlobalEnv)
  
  return(invisible(reclass(x)))
}
