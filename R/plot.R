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
#    legend.loc
#    COLOR GRADIENT FOR SCATTERPLOT CASE
#    Combine OHLC and multi-panel (i.e., if passed cbind(SPY, AGG)) 
#    Smarter OHLC candle width? I like bar width
#    Change default OHLC colors
#    ylab.loc = c("left", "right", "out","in","flip","above") -- above kills panel alignment automatically
#    Refactor plotting functionality into some non-exported bits
#    It stopped handling ylab when I did the axis hardcoding -- should be smarter
#    x <- as.xts(sample_matrix); plot(cbind(x, x[,1]), layout = matrix(1:6, ncol = 2)) -- is this a bug?: JMU
#    Option to have fixed y-scale throughout
#    xlim smart subsetting should be panel-wise

## How I really want to handle screens
## Give user ultimate flexibility in setting up screens combining them as desired with layout-like interface
## Go by rows on matrix and whenever number of panels changes, add new time axis
## E.g. layout(matrix(c(1,1,1,1), ncol = 2) has one time axis 
## E.g. layout(matrix(c(1,2,1,2), ncol = 2) has one time axis
## E.g. layout(matrix(c(1,2,1,3), ncol = 2) has three time axes -- one underneath the first set of panels, two more for each of the second row
## E.g. layout(matrix(c(1,2,3,1,4,5), ncol=2) has three time axes -- one underneath the first set of panels, two more for each of the third row [since shared with second]

`plot.xts` <- function(x, y = NULL, 
                       screens = 'auto', layout.screens = 'auto',
                       auto.grid=TRUE,
                       major.ticks='auto', minor.ticks=TRUE, 
                       major.format=TRUE,
                       bar.col='black', candle.col='red',
                       xy.labels = FALSE, xy.lines = NULL,
                       ...) {
  
  # Restore old par() options from what I change in here
  old.par <- par(no.readonly = TRUE)
  
  on.exit(par(old.par))
  on.exit(assign(".plot.xts", recordPlot(), .GlobalEnv), add = TRUE)
  
  dots <- list(...)
   
  ## if y supplied: scatter plot y ~ x
  if(!is.null(y)) {
    
    xlab <- if("xlab" %in% names(dots)) dots[["xlab"]] else deparse(substitute(x))
    ylab <- if("ylab" %in% names(dots)) dots[["ylab"]] else deparse(substitute(y))
    
    if(NCOL(x) > 1 || NCOL(y) > 1) stop("Scatter plots only for univariate series")
    
    return(do_scatterplot(x, y, xy.labels, xy.lines, xlab, ylab, ...))
  }
  
  ## Else : no y, only x
  
  # Need to catch this one early before try.xts forces evaluation
  main <- if(!("main" %in% names(dots))) deparse(substitute(x)) else dots[["main"]]
  
  x <- try.xts(x)
						   
  if("xlim" %in% names(dots)){
    xlim <- dots[["xlim"]]
	  
    if(is.timeBased(xlim)){
      if(length(xlim) != 2L) stop("Need endpoints only for xlim")
      xlim <- do.call(paste0("as.",indexClass(x))[1], list(xlim))
      x <- x[(index(x) > xlim[1]) & (index(x) < xlim[2]), , drop = FALSE]
    }
    if(is.numeric(xlim)){
      warning("Using xlim as row indices -- provide timeBased xlim if you want to subset that way")
      x <- x[xlim[1]:xlim[2], drop = FALSE]
    }
    if(is.character(xlim)){  
      x <- x[xlim, , drop = FALSE]
    }
  }
  
  # Catch OHLC case independently
  if("type" %in% names(dots) && dots[["type"]] %in% c('candles','bars')){
    
    type <- dots[["type"]]
    
    if(!xts:::is.OHLC(x)) stop(type, '-chart not supported for non-OHLC series')
    
    do_plot.ohlc(x, bar.col = bar.col, candle.col = candle.col, 
          major.ticks = major.ticks, minor.ticks = minor.ticks, 
          auto.grid = auto.grid, major.format = major.format, main = main, candles = (type == "candles"), ...)
  } else {  
    # Else need to do layout plots
    screens <- do_layout(x, screens = screens, layout.screens = layout.screens)
    
    have_x_axis <- screens[["have_x_axis"]]
    have_y_axis <- screens[["have_y_axis"]]
    screens <- screens[["screens"]]
    
    x.split <- split.xts.by.cols(x, screens)
    
    # For now, loop over screens one by one constructing relevant elements
    for(i in seq_along(levels((screens)))){
      x.plot <- x.split[[i]]
    
      # Set Margins for each panel here!
      
      # Handle the screen-wise parameters here
      if("ylab" %in% names(dots)) {
        ylab.panel <- get.elm.recycle(dots[["ylab"]],i)
      } else {
        ylab.panel <- if(!is.null(colnames(x.plot)[[1]])) colnames(x.plot)[[1]] else ""
      }
        
      if("log" %in% names(dots)){
        log.panel <- get.elm.recycle(dots[["log"]],i)
      } else {
        log.panel <- ""
      }
      
      # Note that do_add.grid also sets up axes and what not
      do_add.grid(x.plot, major.ticks, major.format, minor.ticks, 
                auto.grid = auto.grid, ylab = ylab.panel, log = log.panel, 
                  have_x_axis = have_x_axis[i], have_y_axis = have_y_axis[i])
      
      col.panel  <- get.elm.from.dots("col", dots, screens, i)
      pch.panel  <- get.elm.from.dots("pch", dots, screens, i)
      cex.panel  <- get.elm.from.dots("cex", dots, screens, i)
      lwd.panel  <- get.elm.from.dots("lwd", dots, screens, i)
      type.panel <- get.elm.from.dots("type", dots, screens, i)
      
      do_add.lines(x.plot, col = col.panel, lwd = lwd.panel, pch = pch.panel, 
                   type = type.panel, cex = cex.panel)
    }
    
  }  
  title(main, outer = length(levels(screens)) > 1L)
  assign(".plot.xts",recordPlot(),.GlobalEnv)
  return(invisible(reclass(x)))
}

do_scatterplot <- function(x, y, xy.labels, xy.lines, xlab, ylab, main, log, cex, xlim, ylim, type, pch, ...){
  if(missing(main)) main <- paste(xlab, "vs.", ylab)
  if(missing(log))  log  <- ''
  if(missing(cex))  cex  <-  0.8
  if(missing(pch))  pch  <- 1L
  
  x <- try.xts(x); y <- try.xts(y)
  
  xy.xts <- merge(x, y, join = "inner")
  
  xy <- coredata(xy.xts)
  
  xy <- xy.coords(xy[,1], xy[,2])
  
  if(missing(xlim)) xlim <- range(xy$x[is.finite(xy$x)])
  if(missing(ylim)) ylim <- range(xy$y[is.finite(xy$y)])

  do.lab <- if(is.logical(xy.labels)) xy.labels else TRUE

  if(is.null(xy.lines)) xy.lines <- do.lab

  ptype <- if(missing(type)){if(do.lab) "n" else "p"} else type
  type  <- if(missing(type)){if(do.lab) "c" else "l"} else type

  plot(xy[1:2], type = ptype, main = main, xlab = xlab, 
        ylab = ylab, xlim = xlim, ylim = ylim, log = log, pch = pch)

  if(do.lab) text(xy[1:2], cex = cex, labels = if(!is.logical(xy.labels)) xy.labels else index2char(index(xy.xts)))
  if(xy.lines) lines(xy[1:2], type = type)

  return(invisible(xy.xts))
}

do_layout <- function(x, screens, layout.screens){
  # By default one screen per panel
  screens <- factor(if(identical(screens,"auto")) 1:NCOL(x) else 
    rep(screens, length.out = NCOL(x)))
  
  if(identical(layout.screens, "auto")){
    layout.screens <- seq_along(levels(screens))
  }
  
  layout.screens <- as.matrix(layout.screens)
  
  # Would like to use do.call and as.list so pro-users can pass widths and heights
  # to layout -- currently undocumented behavior
  # do.call("layout", as.list(layout.screens)) 
  layout(layout.screens)
  
  have_x_axis <- logical(length(levels(screens)))
  for(i in seq_len(NROW(layout.screens))){
    if(i == NROW(layout.screens)){
      have_x_axis[layout.screens[i,]] <- TRUE
    } else {
      if(!identical(as.logical(diff(layout.screens[i,])), as.logical(diff(layout.screens[i+1,])))){
        have_x_axis[layout.screens[i,]] <- TRUE  
      }
    }
  }
  
  have_y_axis <- logical(length(levels(screens)))
  for(i in seq_len(NCOL(layout.screens))){
    if(i == 1){
      have_y_axis[layout.screens[,i]] <- TRUE
    } else {
      if(!identical(as.logical(diff(layout.screens[,i-1])), as.logical(diff(layout.screens[,i])))){
        have_y_axis[layout.screens[,i]] <- TRUE  
      }
    }
  }
  
  if(length(levels(screens)) > 1L) par(mar = c(0,0,0,0), oma = c(4, 6, 4, 4))
  
  #####
  #
  #  SOME CODE TO MAKE SURE screens.layout IS LEGAL ? 
  #
  #####
  
  # TODO: return boolean of where x-axes labels should go
  return(list(screens = screens, have_x_axis = have_x_axis, have_y_axis = have_y_axis))
}

do_add.grid <- function(x, major.ticks, major.format, minor.ticks, axes, 
                        auto.grid, xlab, ylab, log, have_x_axis, have_y_axis, ...){
  
  # Plotting Defaults
  if(missing(axes)) axes <- TRUE
  if(missing(ylab)) ylab <- ''
  if(missing(xlab)) xlab <- ''
  if(missing(log))  log  <- ''
  
  xy <- list(x = .index(x), y = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = NROW(x)))
  plot(xy$x, xy$y, type = "n", axes=FALSE, xlab = xlab, ylab = ylab, log = log)
  
  ep <- axTicksByTime(x, major.ticks, format.labels = major.format)
  
  if(auto.grid) {
    abline(v=xy$x[ep], col='grey', lty=4)
    grid(NA,NULL)
  }
  
  if(axes) {
    if(have_x_axis){
      if(minor.ticks) axis(1, at=xy$x, labels=FALSE, col='#BBBBBB')
      axis(1, at=xy$x[ep], labels=names(ep), las=1, lwd=1, mgp=c(3,2,0))  
    }
    
    if(have_y_axis){
      axis(2)  
    }
  }
  
  box()
}

do_add.lines <- function(x, col, pch, cex, lwd, type, ...){
  
  if(is.null(col)) col <- 1:NCOL(x)
  if(is.null(pch)) pch <- 1
  if(is.null(cex)) cex <- 1
  if(is.null(lwd)) lwd <- 1
  if(is.null(type)) type <- "l"
  
  for(j in 1:NCOL(x)){
    col.t  <- get.elm.recycle(col, j)
    pch.t  <- get.elm.recycle(pch, j)
    cex.t  <- get.elm.recycle(cex, j)
    lwd.t  <- get.elm.recycle(lwd, j)
    type.t <- get.elm.recycle(type, j)
    
    lines(x[,j], col = col.t, pch = pch.t, type = type.t, lwd = lwd.t, cex = cex.t)
  }
}

do_add.shading <- function(){}

do_add.event <- function(){}

do_add.legend <- function(){}

do_plot.ohlc <- function(x, bar.col, candle.col, major.ticks, 
                                 minor.ticks, major.format, auto.grid, candles, ...){
  
  if(QUANTMOD_MESSAGE) {
    message("Note that CRAN Package quantmod provides much better OHLC charting.\n",
            "This message will show once per sesion.")
    assignInMyNamespace("QUANTMOD_MESSAGE",FALSE) 
    # Is there a better way to do this ? 
    # I'd think lexical scoping but I get a locked binding error
  }
  
  # Extract OHLC Columns and order them
  x <- x[,xts:::has.OHLC(x, TRUE)] 
  
  do_add.grid(x, major.ticks = major.ticks, major.format = major.format, 
              minor.ticks = minor.ticks, auto.grid = auto.grid, 
              have_x_axis = TRUE, have_y_axis = TRUE, ...)
  
  width = .2*deltat(x)
  
  # Better to do this with xts:::Op etc when moved to xts package
  
  # Candles -- not happy about lwd fixed: make dynamic / smart?
  if(candles) segments(.index(x), x[,2L], .index(x), x[,3L], col = candle.col)
  
  # Bars for all OHLC
  rect(.index(x) - width, x[, 1L], .index(x) + width, x[, 4L], 
       col = bar.col, border = bar.col)
  
  return(invisible(reclass(x)))
}

# split.xts which returns an xts instead of a zoo
split.xts.by.cols <- function(x, f){
  lapply(split(seq_len(NCOL(x)), f), function(cols) x[,cols])
}
  
get.elm.recycle <- function(vec, n){
  j <- n %% length(vec) 
  vec[[if(j) j else length(vec)]]
}

get.elm.from.dots <- function(par, dots, screens, n){
  if(!(par %in% names(dots))) NULL else 
    get.elm.recycle(split(rep(if(length(levels(screens)) == 1L) list(dots[[par]]) else dots[[par]],
     length.out = length(screens)), screens), n)
}

QUANTMOD_MESSAGE <- TRUE # Suggests quantmod to user of OHLC plot.xts