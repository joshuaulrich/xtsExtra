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

# To do: 
#    REMOVE par() ARGS FROM FORMALS AND INSTEAD TREAT ... BETTER
#    DO LAYOUT WITHOUT USING LAYOUT -- NEED TO BE ABLE TO MOVE BETWEEN PLOTS WHEN ADDING LINES?
#    GET LAYOUT TO SUPPORT ADJACENT COLUMNS
#    HANDLE xlim AS ISO8601 AS WELL
#    legend.loc
#    COLOR GRADIENT FOR SCATTERPLOT CASE
#    Combine OHLC and multi-panel (i.e., if passed cbind(SPY, AGG)) 
#    Get OHLC to support log = 
#    candle.col is not supported? 

`plot.xts` <- function(x, y = NULL, 
                       screens, screens.layout,
                       type, auto.grid=TRUE,
                       major.ticks='auto', minor.ticks=TRUE, 
                       major.format=TRUE,
                       bar.col='grey', candle.col='white',
                       xy.labels = FALSE, xy.lines = NULL, 
                       ...) {
  
  dots <- list(...)
  
  setPar <- function(x, default){
    # See if par was passed through ... 
    # if so, use it, else use default
    # Also strip from dots once it's been handled directly
    
    # Other model would be to modify it in dots and pass that way
    
    x <- deparse(substitute(x))
    
    r <- if(x %in% names(dots)){
      dots[[x]]
    } else {
      default
    }
    dots[[x]] <- NULL
    assign("dots", dots, parent.frame())
    
    r
  }  
  
  ## if y supplied: scatter plot y ~ x
  if(!is.null(y)) {
    if(NCOL(x) > 1 || NCOL(y) > 1) stop("Scatter plots only for univariate series")
    
    # Am I catching these early enough
    xlab <- setPar(xlab, deparse(substitute(x)))
    ylab <- setPar(ylab, deparse(substitute(y)))
    main <- setPar(main, paste(xlab, "vs.", ylab))
    
    x <- try.xts(x); y <- try.xts(y)
    
    xy.xts <- merge(x, y, join = "inner")
    
    xy <- coredata(xy.xts)
    
    xy <- xy.coords(xy[,1], xy[,2])

    xlim <- setPar(xlim, range(xy$x[is.finite(xy$x)]))
    ylim <- setPar(xlim, range(xy$y[is.finite(xy$y)]))
    
    do.lab <- if(is.logical(xy.labels)) xy.labels else TRUE
    
    if(is.null(xy.lines)) xy.lines <- do.lab
    
    ptype <- setPar(type, if(do.lab) "n" else "p")

    do.call("plot.default", c(xy[1:2], list(type = ptype, main = main, xlab = xlab, 
      ylab = ylab, xlim = xlim, ylim = ylim), dots))
      
    if(do.lab) do.call("text", 
      c(xy[1:2], list(labels = if(!is.logical(xy.labels)) xy.labels else index2char(index(xy.xts))), dots))
    if(xy.lines) do.call("lines", c(xy[1:2], list( type = if(do.lab) "c" else "l"), dots))

    assign(".plot.xts", recordPlot(), .GlobalEnv)
    return(invisible(xy.xts))
  }
  
  ## Else : no y, only x
  main <- setPar(main, deparse(substitute(x)))
  xlab <- setPar(xlab, '')
  log <- setPar(log, '')
  axes <- setPar(axes, TRUE)
  
  x <- try.xts(x)
  
  # Catch OHLC case independently -- will violate DRY but that's ok for now
  if(!missing(type) && type %in% c('candles','bars') && xts:::is.OHLC(x)){
  
    if(type == 'bars') stop('OHLC bars not yet supported.')
  
    # Handle OHLC candles 
    x <- x[,xts:::has.OHLC(x, TRUE)] 
    ylab <- setPar(ylab, '')
    
    ep <- axTicksByTime(x, major.ticks, format.labels = major.format)
    
    xy <- list(x = .index(x), y = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = NROW(x)))
    do.call("plot", c(list(x = xy$x, y = xy$y, type = "n", axes=FALSE, xlab = xlab, ylab = ylab, main = main), dots))
    
    if(auto.grid) {
      abline(v=xy$x[ep], col='grey', lty=4)
      grid(NA,NULL)
    }
    
    if(axes) {
      if(minor.ticks)
        axis(1, at=xy$x, labels=FALSE, col='#BBBBBB')
      axis(1, at=xy$x[ep], labels=names(ep), las=1, lwd=1, mgp=c(3,2,0)) 
      axis(2)
    }
    
    print(candle.col)
    
    xts:::plot.ohlc.candles(x, bar.col = bar.col, candle.col = candle.col)
    box()
    assign(".plot.xts",recordPlot(),.GlobalEnv) 
    return(invisible(reclass(x)))
  }
  
  # Else need to do layout plots
  ylab <- setPar(ylab, if(NCOL(x) == 1) "" else 
    if(!is.null(colnames(x))) colnames(x) else paste("Column", seq_len(NCOL(x))))
  
  # By default one screen per panel
  if(missing(screens)){
    screens <- 1:NCOL(x)
  } else {
    screens <- factor(screens)
  }

  if(missing(screens.layout)){
    screens.layout <- seq_along(unique(screens))
  }
  
  #####
  #
  #  SOME CODE TO MAKE SURE screens.layout IS LEGAL
  #
  #####
  
  ep <- axTicksByTime(x,major.ticks, format.labels=major.format)
  type <- if(missing(type)) rep('l', NCOL(x)) else rep(type, length.out = NCOL(x))

  layout(screens.layout) 
  
  # For now, loop over screens and do plots automatically
  for(scrn in unique(screens)){
    x.temp <- x[, which(screens == scrn)]
     
    xy <- list(x = .index(x.temp), y = seq(min(x.temp, na.rm = TRUE), max(x.temp, na.rm = TRUE), length.out = NROW(x)))
    do.call("plot", c(xy[1:2], list(type = "n", axes=FALSE, xlab = "", ylab = ""), dots))
     
    if(auto.grid) {
      abline(v=xy$x[ep], col='grey', lty=4)
      grid(NA,NULL)
    }
    
    if(axes) {
      if(minor.ticks)
        axis(1, at=xy$x, labels=FALSE, col='#BBBBBB')
      axis(1, at=xy$x[ep], labels=names(ep), las=1, lwd=1, mgp=c(3,2,0)) 
      axis(2)
    }
    
    for(col in seq_len(NCOL(x.temp))){
       lines(x.temp[, col], type = "l")
    }
    
    box() 
  }
  
  assign(".plot.xts",recordPlot(),.GlobalEnv)
  
  return(invisible(reclass(x)))
}

