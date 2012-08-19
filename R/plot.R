#   xtsExtra: Extensions to xts during GSOC-2012
#
#   Copyright (C) 2012  Michael Weylandt: michael.weylandt@gmail.com
#
#   Scatterplot code taken from plot.zoo in the CRAN zoo package 
#   Thanks to  A. Zeilis & G.Grothendieck
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

`plot.xts` <- function(x, y = NULL, screens = 'auto', layout.screens = 'auto',
      yax.loc = c("none", "out", "in", "flip", "left", "right", "top"), 
      auto.grid = TRUE, major.ticks = 'auto', minor.ticks = TRUE, major.format = TRUE, 
      bar.col.up = 'white', bar.col.dn ='red', candle.col='black',
      xy.labels = FALSE, xy.lines = NULL, ylim = 'auto', panel = default.panel,
      auto.legend = FALSE, legend.names = colnames(x), legend.loc = "topleft", 
      legend.pars = NULL, events, blocks, nc, nr, ...) {
  
  # Set cex.lab early to a reasonable default; this allows user to still override
  if(length(screens) > 1L || (NCOL(x) > 1L && identical(screens, "auto"))) par(cex.lab = 0.8)
  
  # Restore old par() options from what I change in here
  old.par <- par(no.readonly = TRUE)
  
  on.exit(par(old.par))
  on.exit(assign(".plot.xts", recordPlot(), .GlobalEnv), add = TRUE)
  
  dots <- list(...)
  
  do.call(par, dots[!(names(dots) %in% 
    c("col", "type", "lwd", "pch", "log", "cex", "ylab", "main", "axes", "xlab", "lty"))])
   
  ## if y supplied: scatter plot y ~ x
  if(!is.null(y)) {
    
    xlab <- if("xlab" %in% names(dots)) dots[["xlab"]] else deparse(substitute(x))
    ylab <- if("ylab" %in% names(dots)) dots[["ylab"]] else deparse(substitute(y))
    
    if(NCOL(x) > 1L || NCOL(y) > 1L){
      layout(matrix(seq_len(NCOL(x)*NCOL(y)),ncol = NCOL(x), nrow = NCOL(y)))
      par(mar = c(2,2,2,2), oma = c(0,0,3,0))
      
      for(i in seq_len(NCOL(x))){
        for(j in seq_len(NCOL(y))){
          do_scatterplot(x[,i], y[,j], xy.labels, xy.lines, xlab = "", ylab = "", ..., 
                         main = paste(names(x)[i],"vs.",names(y)[j]))  
        }
      }
      
      mtext(paste(xlab, "vs.", ylab), outer = TRUE, line = 0)
      
      return(invisible(merge(x,y)))
    } else {
      return(do_scatterplot(x,y, xy.labels, xy.lines, xlab, ylab, ...))
    }
  }
  
  ## Else : no y, only x
  
  # Need to catch this one early before try.xts forces evaluation
  main <- if(!("main" %in% names(dots))) deparse(substitute(x)) else dots[["main"]]
  
  x <- try.xts(x)
						   
  if("xlim" %in% names(dots)){
    xlim <- dots[["xlim"]]
	  
    if(is.timeBased(xlim)){
      if(length(xlim) != 2L) stop("Need endpoints only for xlim")
      xlim <- do.call(paste0("as.",indexClass(x))[1L], list(xlim))
      x <- x[(index(x) > xlim[1L]) & (index(x) < xlim[2L]), , drop = FALSE]
    }
    if(is.numeric(xlim)){
      warning("Using xlim as row indices -- provide timeBased xlim",
              "if you wish to subset that way")
      x <- x[xlim[1L]:xlim[2L], drop = FALSE]
    }
    if(is.character(xlim)){  
      x <- x[xlim, , drop = FALSE]
    }
  }
  
  yax.loc <- match.arg(yax.loc)
  
  # Catch OHLC case independently
  if("type" %in% names(dots) && dots[["type"]] %in% c('candles','bars')){
    
    type <- dots[["type"]]
    
    if(!xts:::is.OHLC(x)) stop(type, '-chart not supported for non-OHLC series')
    
    do_plot.ohlc(x, bar.col.up = bar.col.up, bar.col.dn = bar.col.dn, 
                 candle.col = candle.col, major.ticks = major.ticks, 
                 minor.ticks = minor.ticks, auto.grid = auto.grid, 
                 major.format = major.format, main = main, 
                 candles = (type == "candles"), events = events, 
                 blocks = blocks, yax.loc = yax.loc, ylim = ylim, ...)
    
  } else {  
    # Else need to do layout plots
    screens <- do_layout(x, screens = screens, layout.screens = layout.screens, 
                         yax.loc = yax.loc, nc = nc, nr = nr, ylim = ylim)
    
    layout.screens <- screens[["layout.screens"]]
    have_x_axis <- screens[["have_x_axis"]]
    have_y_axis <- screens[["have_y_axis"]]
    ylab.axis <- screens[["ylab.axis"]]
    ylim <- screens[["ylim"]]
    screens <- screens[["screens"]]
    
    x.split <- split.xts.by.cols(x, screens)
    
    if(auto.legend) legend.names <- split(legend.names, screens)
    
    # For now, loop over screens one by one constructing relevant elements
    for(i in seq_along(levels((screens)))){
      x.plot <- x.split[[i]]
    
      col.panel  <- get.elm.from.dots("col",  dots, screens, i)
      pch.panel  <- get.elm.from.dots("pch",  dots, screens, i)
      cex.panel  <- get.elm.from.dots("cex",  dots, screens, i)
      lwd.panel  <- get.elm.from.dots("lwd",  dots, screens, i)
      type.panel <- get.elm.from.dots("type", dots, screens, i)
      lty.panel  <- get.elm.from.dots("lty",  dots, screens, i)
      
      # Set these defaults here
      ylab.panel <- get.elm.from.dots("ylab", dots, screens, i)[[1L]]
      if(is.null(ylab.panel)) ylab.panel <- if(!is.null(colnames(x.plot)[[1L]])) colnames(x.plot)[[1L]] else ""
      
      log.panel <- get.elm.from.dots("log", dots, screens, i)
      if(is.null(log.panel)) log.panel <- ""
      
      panel.panel <- match.fun(if(length(panel) > 1L) get.elm.recycle(panel, i) else panel)
      
      # Note that do_add.grid also sets up axes and what not
      do_add.grid(x.plot, major.ticks, major.format, minor.ticks, 
            auto.grid = auto.grid, ylab = ylab.panel, log = log.panel, 
            have_x_axis = have_x_axis[i], have_y_axis = have_y_axis[i],
            ylab.axis = ylab.axis[which.max(layout.screens == i)], # Use which.max to get first hit 
            events = events, blocks = blocks,
            yax.loc = yax.loc, ylim = get.elm.recycle(ylim, i))
      
      legend.pars.add <- do_add.panel(x.plot, panel = panel.panel, col = col.panel, lwd = lwd.panel, 
                   pch = pch.panel, type = type.panel, cex = cex.panel, lty = lty.panel)

      if(auto.legend && !is.na(get.elm.recycle(legend.loc,i)))
        do.call(do_add.legend, c(legend.names = list(legend.names[[i]]), 
                    legend.loc = get.elm.recycle(legend.loc, i), 
                    legend.pars.add, legend.pars))
    }
    
  }  
  title(main, outer = TRUE) # outer = length(levels(screens)) > 1L)
  return(invisible(reclass(x)))
}

do_scatterplot <- function(x, y, xy.labels, xy.lines, xlab, ylab, main, 
                           log, cex, xlim, ylim, type, pch, col, ...){
  
  if(missing(main)) main <- paste(xlab, "vs.", ylab)
  if(missing(log))  log  <- ''
  if(missing(cex))  cex  <- 0.8
  if(missing(pch))  pch  <- 1L
  if(missing(col))  col  <- 1L
  
  x <- try.xts(x); y <- try.xts(y)
  
  xy.xts <- merge(x, y, join = "inner")
  
  xy <- coredata(xy.xts)
  
  xy <- xy.coords(xy[,1L], xy[,2L])
  
  if(missing(xlim)) xlim <- range(xy$x[is.finite(xy$x)])
  if(missing(ylim)) ylim <- range(xy$y[is.finite(xy$y)])

  do.lab <- if(is.logical(xy.labels)) xy.labels else TRUE

  if(is.null(xy.lines)) xy.lines <- do.lab

  ptype <- if(missing(type)){if(do.lab) "n" else "p"} else type
  type  <- if(missing(type)){if(do.lab) "c" else "l"} else type

  plot(xy[1:2], type = ptype, main = main, xlab = xlab, 
        ylab = ylab, xlim = xlim, ylim = ylim, log = log, pch = pch, col = col)

  if(do.lab) text(xy[1:2], cex = cex, labels = if(!is.logical(xy.labels)) 
    xy.labels else index2char(index(xy.xts)), col = col)
  
  if(xy.lines) segments(xy[[1L]][-NROW(xy[[1L]])],xy[[2L]][-NROW(xy[[2L]])], 
                     xy[[1L]][-1L],xy[[2L]][-1L], col = col)

  return(invisible(xy.xts))
}

do_layout <- function(x, screens, layout.screens, yax.loc, nc, nr, ylim){
  # By default one screen per panel
  screens <- factor(if(identical(screens,"auto")) seq_len(NCOL(x)) else 
    rep(screens, length.out = NCOL(x)))
  
  if(identical(layout.screens, "auto")){
    layout.screens <- seq_along(levels(screens))
    if(!missing(nc) && !missing(nr)) 
      layout.screens <- matrix(layout.screens, ncol = nc, nrow = nrow)
    if(missing(nc) && !missing(nr))
      layout.screens <- matrix(layout.screens, nrow = nr)
    if(!missing(nc) && missing(nr))
      layout.screens <- matrix(layout.screens, ncol = nc)
  }
  
  if(is.list(layout.screens)) {
    layout.args <- layout.screens[-1L]
    layout.screens <- layout.screens[[1L]]
  }
  
  layout.screens <- as.matrix(layout.screens)
  
  have_x_axis <- logical(length(levels(screens)))
  for(i in seq_len(NROW(layout.screens))){
    if(i == NROW(layout.screens)){
      have_x_axis[layout.screens[i,]] <- TRUE
    } else {
      if(!identical(as.logical(diff(layout.screens[i, ])), 
                    as.logical(diff(layout.screens[i + 1L,])))){
        have_x_axis[layout.screens[i,]] <- TRUE  
      }
    }
  }
  
  have_y_axis <- logical(length(levels(screens)))
  for(i in seq_len(NCOL(layout.screens))){
    if(i == 1L){
      have_y_axis[layout.screens[,i]] <- TRUE
    } else {
      if(!identical(as.logical(diff(layout.screens[ ,i - 1L])), 
                    as.logical(diff(layout.screens[ ,i])))){
        have_y_axis[layout.screens[,i]] <- TRUE  
      }
    }
  }
  
  # Here we handle y-axis labeling case by case and mark when margins are needed
  # From this part, we return a vector ylab.axis giving L/R/None marks for y-labels
  # Margins are set appropriately back in main function body
  
  ylab.axis <- layout.screens
  
  if(yax.loc == "none") ylab.axis[] <- "none"
  
  # If labels are set to left/right we need them in all panels
  if(yax.loc == "right" || yax.loc == "left") {
    have_y_axis[] <- TRUE # Since forcing labels, we write a y-axis everywhere
    
    ylab.axis[] <- yax.loc
  }
    
  if(yax.loc == "out" || yax.loc == "in"){
    if(NCOL(layout.screens) != 2L) stop("yax.loc not consistent with layout -- too many columns.")
    # If labels are set to out we need them for outer panels only
    # If labels are set to in we need them for inner panels only
    ylab.axis[,1L] <- if(yax.loc == "out") "left" else "right"
    ylab.axis[,2L] <- if(yax.loc == "out") "right" else "left"
    have_y_axis[] <- TRUE # Axes for all if TRUE
  }
  
  # If labels are set to flip we do a little bit of work to arrange them
  if(yax.loc == "flip") {
    for(i in seq_len(NCOL(ylab.axis))) 
      ylab.axis[,i] <- rep(c("left","right"), length.out = NROW(ylab.axis))
    have_y_axis[] <- TRUE
  }
  
  if(yax.loc == "top"){
    ylab.axis[] <- yax.loc
    have_y_axis[] <- TRUE
    have_x_axis[] <- TRUE
  }
  
  # Moving internal margin code to the panel-wise setup, leaving oma (outer) margin here
  if(length(levels(screens)) > 1L) par(oma = c(1,1,4,1))
  if(yax.loc == "none") par(oma = c(1,4,4,3))
  if(length(levels(screens)) == 1L && yax.loc != "none") par(oma = c(1,1,4,1))
  
  if(identical(ylim,'fixed')){
    ylim <- list(range(x))
  } else if(identical(ylim, 'auto')){
    if(yax.loc == "none") {
      ylim <- lapply((1:NROW(layout.screens))[!duplicated(layout.screens)], function(y) {
        do.call(range,split.xts.by.cols(x, screens)[layout.screens[y,]])
      })
    } else {
      ylim <- lapply(split.xts.by.cols(x, screens), range)
    }
  } else{
    if(!is.matrix(ylim)) dim(ylim) <- c(1L, NROW(ylim))
    ylim <- lapply(1:NROW(ylim), function(x) ylim[x,1:2])
  }
  
  # Would like to use do.call and as.list so pro-users can pass widths and heights
  # to layout -- currently undocumented behavior
  # do.call("layout", as.list(layout.screens)) 
  # Currently I add a little bit extra height to those screens with x-axes
  if(length(layout.screens) > 1L){
    if(!exists("layout.args")) {
      layout(layout.screens, heights = 1 + 0.05*NROW(unique(layout.screens)) * 
        apply(layout.screens, 1L ,function(j) any(have_x_axis[j])))
      # More dirty hacking.... still not perfect
    } else {
      do.call(layout, c(list(layout.screens), layout.args))
    }
  }
    
  return(list(layout.screens = layout.screens, screens = screens, have_x_axis = have_x_axis, 
              have_y_axis = have_y_axis, ylab.axis = ylab.axis, ylim = ylim))
}

do_add.grid <- function(x, major.ticks, major.format, minor.ticks, axes, 
                        auto.grid, xlab, ylab, log, have_x_axis, have_y_axis, 
                        ylab.axis, events, blocks, yax.loc, ylim, ...){

  # Set Margins for each panel here!
  if(yax.loc == "flip"){
    par(mar = have_x_axis*c(3.4, 0, 0, 0) + c(0, 4.5, 0, 4.5))
  } else {
    par(mar = have_x_axis*c(3.4,0,0,0) +
         switch(ylab.axis,
              none  = c(0,   0, 0,   0),
              left  = c(0, 4.5, 0, 1.5), 
              right = c(0, 1.5, 0, 4.5),
              top   = c(0, 4.5, 1.5, 1.5)))
  }
  # Plotting Defaults
  if(missing(axes)) axes <- TRUE
  if(missing(ylab)) ylab <- ''
  if(missing(xlab)) xlab <- ''
  if(missing(log))  log  <- ''
  
  xy <- list(x = .index(x), y = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = NROW(x)))
  
  plot(xy$x, xy$y, type = "n", axes = FALSE, xlab = xlab, ylab = '', log = log, ylim = ylim)
  
  mtext(side = 2 + 2*(ylab.axis == "right") + 1*(ylab.axis == "top"), text = if(ylab.axis == "none") "" else ylab, 
        line = 3 - 2.5*(ylab.axis == "top"), cex = par("cex.lab"), col = par("col.lab"))
  ep <- axTicksByTime(x, major.ticks, format.labels = major.format)
  
  if(!missing(blocks)){
    do_add.shading(blocks, ylim)
  }
  
  if(!missing(events)){
    do_add.event(events, ylim)
  }
  
  if(auto.grid) {
    abline(v = xy$x[ep], col = 'grey', lty = 4L)
    grid(NA, NULL)
  }
  
  if(axes) {
    if(have_x_axis){
      if(minor.ticks) axis(1L, at = xy$x, labels = FALSE, col = par("col.axis"))
      axis(1L, at = xy$x[ep], labels = names(ep), lwd = 1L, 
           mgp = c(3, 2, 0), col = par("col.axis"))  
      # Not sure why I have to force col.axis but it seems I do
    }
    if(have_y_axis){
      axis(2L + 2L*(ylab.axis == "right"), col = par("col.axis"))  
    }
  }
  
  box()
}

do_add.panel <- function(x, col, pch, cex, lwd, type, panel, lty, ...){
  
  if(is.null(col))  col <- seq_len(NCOL(x))
  if(is.null(pch))  pch <- 1L
  if(is.null(cex))  cex <- 1L
  if(is.null(lwd))  lwd <- 1L
  if(is.null(type)) type <- "l"
  if(is.null(lty))  lty <- 1L
  
  panel(.index(x), x, col = col, pch = pch, type = type, 
          lwd = lwd, cex = cex, lty = lty)
  
  list(col = col, pch = pch, cex = cex, lwd = lwd, type = type, lty = lty)
}

do_add.shading <- function(blocks, y){
  yrng <- c(0, -3*max(y), 3*min(y), 3*max(y), -3*min(y)) # Dirty Hack
  
  for(j in seq_along(blocks[["start.time"]])){    
    rect(as.POSIXct(get.elm.recycle(blocks[["start.time"]], j)), max(yrng),
         as.POSIXct(get.elm.recycle(blocks[["end.time"]], j)), min(yrng),
         col = if(!is.null(blocks[["col"]])) 
           get.elm.recycle(blocks[["col"]],j) else "lightblue1",
         border = NA)
  }
}

do_add.event <- function(events, y){
  
  getFromEvents <- function(prop, j, default){
    if(!is.null(events[[prop]])) get.elm.recycle(events[[prop]],j) else default
  }
  
  for(j in seq_along(events[["time"]])){
    
    time  <-  as.POSIXct(get.elm.recycle(events[["time"]],j))
    label <-  get.elm.recycle(events[["label"]], j)
    
    col <-  getFromEvents("col", j, "red")
    lty <-  getFromEvents("lty", j, 2)
    
    y.adj  <- getFromEvents("y.adj", j, 0)
    offset <- getFromEvents("offset", j, 0.2)
    pos    <- getFromEvents("pos", j, 2)
    
    text(x = time, y = max(y) - y.adj, label = label, 
         offset = offset, pos = pos, srt = 90, col = col)
    abline(v = time, col = col, lty = lty)
  }
}

do_add.legend <- function(legend.names, legend.loc, col, lwd, pch, cex, type, lty, ...){
  do.call(legend, list(
    x = legend.loc, 
    legend = legend.names, 
    col = col, 
    lwd = ifelse(!(type %in% c("n","p")), lwd,  NA), 
    pch = ifelse(type %in% c("p","b","o"), pch, NA), 
    cex = cex, 
    lty = lty, ...))
}

do_plot.ohlc <- function(x, bar.col.up, bar.col.dn, candle.col, major.ticks, 
                        minor.ticks, major.format, auto.grid, 
                        candles, events, blocks, yax.loc, ylim, ...){
  
  if(exists(".QUANTMOD_MESSAGE", .GlobalEnv) && get(".QUANTMOD_MESSAGE", .GlobalEnv)) {
    message("Note that CRAN Package quantmod provides much better OHLC charting.\n",
            "This message will show once per session.")
    # Help page says not to use assignInMyNamespace() so we'll do it manually in .GlobalEnv
    # Also, it was only introduced in R 2.15 so probably better to remove it
    assign(".QUANTMOD_MESSAGE", FALSE, envir = .GlobalEnv)
  }
  
  if(identical(ylim, 'auto') || identical(ylim, 'fixed')) ylim <- range(x)
  
  # Extract OHLC Columns and order them
  x <- x[,xts:::has.OHLC(x, TRUE)] 
  par(oma = c(1,4,4,3))
  do_add.grid(x, major.ticks = major.ticks, major.format = major.format, 
              minor.ticks = minor.ticks, auto.grid = auto.grid, 
              have_x_axis = TRUE, have_y_axis = TRUE, ylab.axis = "none",
              events = events, blocks = blocks, yax.loc = yax.loc, ylim = ylim, ...)
  
  width = .2*deltat(x)
  
  # Better to do this with xts:::Op etc when moved to xts package?
  if(candles) rect(.index(x) - width/4, x[,2L], .index(x) + width/4, x[,3L], 
                   col = candle.col, border = candle.col)
  
  # Bars for all OHLC
  rect(.index(x) - width, x[, 1L], .index(x) + width, x[, 4L], 
       col = ifelse(x[,4L] > x[,1L], bar.col.up, bar.col.dn), border = candle.col)
  
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
  # Return NULL if par is not supplied
  if(!(par %in% names(dots))) return(NULL)
  
  # Repeat par to length of screens and take n-th screen
  if(length(screens) == 1L){
    par <- rep(list(dots[[par]]), length.out = length(screens))
  } else {
    par <- rep(dots[[par]], length.out = length(screens))
  }
    
  par <- split(par, screens)
    
  j <- n %% length(par)
  par[[if(j) j else length(par)]]  
}

default.panel <- function(index, x, col, pch, cex, lwd, type, lty){
  # This unexported function exists only to provide a 
  # default panel function within plot.xts 
  for(j in seq_len(NCOL(x))){
    col.t  <- get.elm.recycle(col,  j)
    pch.t  <- get.elm.recycle(pch,  j)
    cex.t  <- get.elm.recycle(cex,  j)
    lwd.t  <- get.elm.recycle(lwd,  j)
    type.t <- get.elm.recycle(type, j)
    lty.t  <- get.elm.recycle(lty,  j)
    lines(index, x[,j], col = col.t, pch = pch.t, type = type.t, 
          lwd = lwd.t, cex = cex.t, lty = lty.t)
  }
}
