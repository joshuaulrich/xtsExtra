#   xtsExtra: Extensions to xts during GSOC-2012
#
#   Copyright (C) 2012  Michael Weylandt: michael.weylandt@gmail.com
#
#   Scatterplot code taken from plot.zoo in the CRAN zoo package 
#   Thanks to  A. Zeilis & G.Grothendieck
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
#    REMOVE par() ARGS FROM FORMALS AND INSTEAD TREAT ... BETTER [Still need to do "type"]
#    I think layout is working, but need to turn off x/y labels smartly when things are adjacent
#    Handle not adjacent cases
#    
#    DO LAYOUT WITHOUT USING LAYOUT -- NEED TO BE ABLE TO MOVE BETWEEN PLOTS WHEN ADDING LINES?
#    GET LAYOUT TO SUPPORT ADJACENT COLUMNS
#    HANDLE xlim AS ISO8601 AS WELL
#    legend.loc
#    COLOR GRADIENT FOR SCATTERPLOT CASE
#    Combine OHLC and multi-panel (i.e., if passed cbind(SPY, AGG)) 
#    candle.col is not supported? 
#    ylab.loc = c("left", "right", "out","in","flip","above") -- above kills panel alignment automatically
#    Refactor plotting functionality into some non-exported bits
#    We've stopped handling ylab? 

## How I really want to handle screens
## Give user ultimate flexibility in setting up screens combining them as desired with layout-like interface
## Go by rows on matrix and whenever number of panels changes, add new time axis
## E.g. layout(matrix(c(1,1,1,1), ncol = 2) has one time axis 
## E.g. layout(matrix(c(1,2,1,2), ncol = 2) has one time axis
## E.g. layout(matrix(c(1,2,1,3), ncol = 2) has three time axes -- one underneath the first set of panels, two more for each of the second row
## E.g. layout(matrix(c(1,2,3,1,4,5), ncol=2) has three time axes -- one underneath the first set of panels, two more for each of the third row [since shared with second]

`plot.xts` <- function(x, y = NULL, 
                       screens, screens.layout,
                       auto.grid=TRUE,
                       major.ticks='auto', minor.ticks=TRUE, 
                       major.format=TRUE,
                       bar.col='grey', candle.col='white',
                       xy.labels = FALSE, xy.lines = NULL,
                       ...) {
  
  # Restore old par() options from what I change in here
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  
  dots <- list(...)
  
  setParGlb <- function(arg, default){
    # See if par was passed through ... 
    # if so, use it, else use default
    #
    # Also strip from dots once it has been handled directly
    # This is for "global" parameters
    # See setParCol for ones which are columnwise (series-wise)
    
    arg <- deparse(substitute(arg))
    
    r <- if(arg %in% names(dots)){
      r <- dots[[arg]]
      dots[[arg]] <- NULL
      assign("dots", dots, parent.frame())
      r
    } else {
      default
    }
  }
  
  setParCol <- function(arg, default, screens){
    # See if par was passed through ...
    # if so, use it, else use default
    #
    # Also strip from dots once it's been handled directly
    # This is for column(series)-wise parameters
    # Returns a list where each list gives the colwise parameters per panel
    # Only used currently for time series columnwise
    arg <- deparse(substitute(arg))
    
    r <- if(arg %in% names(dots)){
       r <- dots[[arg]]
       dots[[arg]] <- NULL
       assign("dots",dots, parent.frame())
       r
    } else {
      return(default)
    }     
    split(rep(r, length.out = length(screens)), screens)    
  }  
  
  setParScr <- function(arg, default, screens){
    # See if par was passed through ...
    # if so, use it, else use default
    #
    # Also strip from dots once it's been handled directly
    # This is for column(series)-wise parameters
    # Returns a list where each list gives the colwise parameters per panel
    # Only used currently for time series screenwise
    arg <- deparse(substitute(arg))
    
    r <- if(arg %in% names(dots)){
       r <- dots[[arg]]
       dots[[arg]] <- NULL
       assign("dots",dots, parent.frame())
       r
    } else {
      default
    }
    rep(r, length.out = length(screens))
  }
  
  ## if y supplied: scatter plot y ~ x
  if(!is.null(y)) {
    if(NCOL(x) > 1 || NCOL(y) > 1) stop("Scatter plots only for univariate series")
    
    # Am I catching these early enough
    xlab <- setParGlb(xlab, deparse(substitute(x)))
    ylab <- setParGlb(ylab, deparse(substitute(y)))
    main <- setParGlb(main, paste(xlab, "vs.", ylab))
    log <- setParGlb(log, '')
    cex <- setParGlb(cex, 0.7)
    
    x <- try.xts(x); y <- try.xts(y)
    
    xy.xts <- merge(x, y, join = "inner")
    
    xy <- coredata(xy.xts)
    
    xy <- xy.coords(xy[,1], xy[,2])

    xlim <- setParGlb(xlim, range(xy$x[is.finite(xy$x)]))
    ylim <- setParGlb(xlim, range(xy$y[is.finite(xy$y)]))
    
    do.lab <- if(is.logical(xy.labels)) xy.labels else TRUE
    
    if(is.null(xy.lines)) xy.lines <- do.lab
    
    ptype <- setParGlb(type, if(do.lab) "n" else "p")
    type <- setParGlb(type, if(do.lab) "c" else "l")

    do.call("plot.default", c(xy[1:2], list(type = ptype, main = main, xlab = xlab, 
      ylab = ylab, xlim = xlim, ylim = ylim, log = log), dots))
      
    if(do.lab) do.call("text", 
      c(xy[1:2], dots, list(cex = cex, labels = if(!is.logical(xy.labels)) xy.labels else index2char(index(xy.xts)))))
    if(xy.lines) do.call("lines", c(xy[1:2], list(type = type), dots))

    assign(".plot.xts", recordPlot(), .GlobalEnv)
    return(invisible(xy.xts))
  }
  
  ## Else : no y, only x
  main <- setParGlb(main, deparse(substitute(x)))
  xlab <- setParGlb(xlab, '')
  axes <- setParGlb(axes, TRUE)
  
  x <- try.xts(x)
  
  # Catch OHLC case independently -- will violate DRY but that seems ok for now
  if("type" %in% names(dots) && dots[["type"]] %in% c('candles','bars')){
    
    type <- setParGlb(type, 'candles') # This default doesn't really matter since we can't get here without it existing already
    
    if(!xts:::is.OHLC(x)) stop(type, '-chart not supported for non-OHLC series')
    if(type == 'bars') stop('OHLC bars not yet supported.')
  
    # Handle OHLC candles 
    x <- x[,xts:::has.OHLC(x, TRUE)] 
    ylab <- setParGlb(ylab, '')
    log <- setParGlb(log, '')
    
    ep <- axTicksByTime(x, major.ticks, format.labels = major.format)
    
    xy <- list(x = .index(x), y = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = NROW(x)))
    do.call("plot", c(list(x = xy$x, y = xy$y, type = "n", axes=FALSE, xlab = xlab, ylab = ylab, main = main, log = log), dots))
    
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
  } else {  
    # Else need to do layout plots
  
    # By default one screen per panel
    screens <- factor(if(missing(screens)) 1:NCOL(x) else rep(screens, length.out = NCOL(x)))
  
    if(missing(screens.layout)){
      screens.layout <- seq_along(levels(screens))
    }
  
    #####
    #
    #  SOME CODE TO MAKE SURE screens.layout IS LEGAL
    #
    #####
  
    ep <- axTicksByTime(x,major.ticks, format.labels=major.format)
  
    col <- setParCol(col, lapply(split(seq_len(NCOL(x)), screens), rank), screens)
    lwd <- setParCol(lwd, split(rep(1, NCOL(x)), screens), screens)
    type <- setParCol(type, split(rep('l', length(screens)), screens))
    
    ylab <- setParScr(ylab, if(NCOL(x) == 1 || length(levels(screens)) == 1) "" else
      if(!is.null(colnames(x))) colnames(x) else paste("Column", seq_len(NCOL(x))), screens)
    log <- setParScr(log, '', screens)
    

    par(mar = c(0,0,0,0), oma = c(4, 6, 4, 4))
    layout(screens.layout) # BETTER TO DO THIS MANUALLY WITH PAR()
  
    # For now, loop over screens and do plots automatically
    for(scrn in seq_along(levels(screens))){
      x.temp <- x[, which(screens == scrn)]
     
      xy <- list(x = .index(x.temp), y = seq(min(x.temp, na.rm = TRUE), max(x.temp, na.rm = TRUE), length.out = NROW(x)))
      do.call("plot", c(xy[1:2], list(type = "n", axes=FALSE, xlab = "", ylab = ylab[scrn], log = log[scrn]), dots))
     
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
    
      for(column in seq_len(NCOL(x.temp))){
         lines(x.temp[, column], type = type[[scrn]][column], col = col[[scrn]][column], lwd = lwd[[scrn]][column])     
      }
    
      box() 
    }
  }
  title(main, outer = TRUE)
  assign(".plot.xts",recordPlot(),.GlobalEnv)
  return(invisible(reclass(x)))
}

#setup.grid <- function(x){
#  # Sets up the axis background for the plot
#}