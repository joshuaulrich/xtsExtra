
# Environment for our xts chart objects
.plotxtsEnv <- new.env()

current.xts_chob <- function() invisible(get(".xts_chob",.plotxtsEnv))

# based on quantmod R/chart_Series.R

# chart_pars {{{
chart_pars <- function() {
  list(cex=0.6, mar=c(3,2,0,2))
} # }}}

chart.lines <- function(x, 
                        type="l", 
                        lty=1,
                        lwd=2,
                        lend=1,
                        colorset=1:10, 
                        up.col=NULL, 
                        dn.col=NULL,
                        legend.loc=NULL,
                        pch=1){
  if(is.null(up.col)) up.col <- "green"
  if(is.null(dn.col)) dn.col <- "red"
  xx <- current.xts_chob()
  if(type == "h"){
    colors <- ifelse(x[,1] < 0, dn.col, up.col)
    # lines(1:NROW(x),x[,1],lwd=2,col=colors,lend=lend,lty=1,type="h")
    # non-equally spaced x-axis
    lines(xx$Env$xycoords$x,x[,1],lwd=2,col=colors,lend=lend,lty=1,type="h")
  } else if(type == "l" || type == "p") {
    if(length(lty) == 1) lty <- rep(lty, NCOL(x))
    if(length(lwd) == 1) lwd <- rep(lwd, NCOL(x))
    for(i in NCOL(x):1){
      # lines(1:NROW(x), x[,i], type=type, lend=lend, col=colorset[i], lty=lty[i], lwd=lwd[i], pch=pch)
      # non-equally spaced x-axis
      lines(xx$Env$xycoords$x, x[,i], type=type, lend=lend, col=colorset[i], lty=lty[i], lwd=lwd[i], pch=pch)
    }
  } else if(type == "bar"){
    # This does not work correctly
    # The geometry of the x-axis and y-axis is way off with stacked bar plot and
    # the x-axis is off for unstacked bar plot
    # We may need a separate function to do this correctly because of the
    # different geometry/dimensions with stacked and unstacked barplots
    positives = negatives = x
    for(column in 1:NCOL(x)){
      for(row in 1:NROW(x)){ 
        positives[row,column] = max(0, x[row,column])
        negatives[row,column] = min(0, x[row,column])
      }
    }
    barplot.default(t(positives), add=TRUE, col=colorset, axisnames=FALSE, axes=FALSE)
    barplot.default(t(negatives), add=TRUE, col=colorset, axisnames=FALSE, axes=FALSE)
  }
  if(!is.null(legend.loc)){
    yrange <- range(x, na.rm=TRUE)
    # nobs <- NROW(x)
    chob.xlim <- xx$Env$xlim
    switch(legend.loc,
           topleft = {
             xjust <- 0
             yjust <- 1
             lx <- chob.xlim[1]
             ly <- yrange[2]
             },
           left = {
             xjust <- 0
             yjust <- 0.5
             lx <- chob.xlim[1]
             ly <- sum(yrange) / 2
             },
           bottomleft = {
             xjust <- 0
             yjust <- 0
             lx <- chob.xlim[1]
             ly <- yrange[1]
             },
           top = {
             xjust <- 0.5
             yjust <- 1
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- yrange[2]
             },
           center = {
             xjust <- 0.5
             yjust <- 0.5
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- sum(yrange) / 2
             },
           bottom = {
             xjust <- 0.5
             yjust <- 0
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- yrange[1]
             },
           topright = {
             xjust <- 1
             yjust <- 1
             lx <- chob.xlim[2]
             ly <- yrange[2]
             },
           right = {
             xjust <- 1
             yjust <- 0.5
             lx <- chob.xlim[2]
             ly <- sum(yrange) / 2
             },
           bottomright = {
             xjust <- 1
             yjust <- 0
             lx <- chob.xlim[2]
             ly <- yrange[1]
           }
           )
    legend(x=lx, y=ly, legend=colnames(x), xjust=xjust, yjust=yjust, 
           fill=colorset[1:NCOL(x)], bty="n")
  }
}

# function from Peter Carl to add labels to the plot window
# add_label <- function(xfrac, yfrac, label, pos=4, ylog, ...) { 
#   u <- par("usr")
#   x <- u[1] + xfrac * (u[2] - u[1]) 
#   y <- u[4] - yfrac * (u[4] - u[3]) 
#   if(ylog){
#     text(x, 10^y, label, pos = pos, ...)
#   } else {
#     text(x, y, label, pos = pos, ...) 
#   }
# }

# chart_Series {{{
#  Updated: 2010-01-15
#
#  chart_Series now uses a new graphical extension
#  called 'replot'.  This enables the accumulation
#  of 'actions', in the form of (unevaluated) R 
#  expressions, to be stored within a replot object.
#  This object is an R closure, which contains
#  all the methods which are needed to perform
#  graphical operations.
#
#  Ideally all behavior is consistent with the
#  original quantmod:::chartSeries, except the
#  undesireable ones.
# 
# chart_Series <- function(x, 
#                          name=deparse(substitute(x)), 
#                          type="candlesticks", 
#                          subset="", 
#                          TA="",
#                          pars=chart_pars(), theme=chart_theme(),
#                          clev=0,
#                          ...)

xtsExtraTheme <- function(){
  theme <-list(col=list(bg="#FFFFFF",
                        label.bg="#F0F0F0",
                        grid="darkgray", #grid="#F0F0F0",
                        grid2="#F5F5F5",
                        ticks="#999999",
                        labels="#333333",
                        line.col="darkorange",
                        dn.col="red",
                        up.col="green", 
                        dn.border="#333333", 
                        up.border="#333333",
                        colorset=1:10),
               shading=1,
               format.labels=TRUE,
               coarse.time=TRUE,
               rylab=TRUE,
               lylab=TRUE,
               grid.ticks.lwd=1,
               grid.ticks.on="months")
  theme
}

#' Time series Plotting
#' 
#' Plotting for xts objects.
#' TODO: description, details, and examples
#' 
#' @param x xts object
#' @param y NULL, not used
#' @param \dots any passthrough parameters to FUN
#' @param subset character vector of length one of the subset range using subsetting as in \code{\link{xts}}
#' @param FUN function to apply to \code{x} and plot
#' @param panels character vector of expressions to plot as panels
#' @param multi.panel TRUE/FALSE or an integer less than or equal to the number 
#' of columns in the data set. If TRUE, each column of the data is plotted in a 
#' separate panel. For example, if \code{multi.panel = 2}, then the data
#' will be plotted in groups of 2 columns and each group is plotted in a 
#' separate panel. 
#' @param colorset color palette to use, set by default to rational choices
#' @param up.col color for positive bars if \code{type="h"}
#' @param dn.col color for positive bars if \code{type="h"}
#' @param type the type of plot to be drawn, same as in \code{\link{plot}}
#' @param lty set the line type, same as in plot
#' @param lwd set the line width, same as in plot
#' @param lend set the line end style, same as in plot
#' @param main main title
#' @param clev level for shading, not currently used
#' @param cex not currently used
#' @param cex.axis
#' @param mar set the margins, same as in par
#' @param srt rotation for the y axis labels
#' @param xaxis.las rotation for the x axis labels
#' @param ylim the range of the y axis
#' @param yaxis.same TRUE/FALSE. If TRUE, the y axis is drawn with the same ylim for multiple panels 
#' @param yaxis.left if TRUE, draws the y axis on the left
#' @param yaxis.right if TRUE, draws the y axis on the right
#' @param grid.ticks.on period to draw the grid ticks on
#' @param grid.ticks.lwd line width of the grid
#' @param grid.ticks.lty line type of the grid
#' @param grid.col color of the grid
#' @param labels.col color of the axis labels
#' @param format.labels not currently used
#' @param shading not currently used
#' @param bg.col not currently used
#' @param grid2 color for secondary x axis grid
#' @param legend.loc places a legend into one of nine locations on the chart: 
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or 
#' center. Default NULL does not draw a legend. 
#' @author Ross Bennett
plot.xts <- function(x, 
                      y=NULL,
                      ...,
                      subset="",
                      FUN=NULL,
                      panels=NULL,
                      multi.panel=FALSE,
                      colorset=1:12,
                      up.col="green",
                      dn.col="red",
                      type="l",
                      lty=1,
                      lwd=2,
                      lend=1,
                      main=deparse(substitute(x)),  
                      clev=0,
                      cex=0.6, 
                      cex.axis=0.9,
                      mar=c(3,2,0,2), 
                      srt=0,
                      xaxis.las=0,
                      ylim=NULL,
                      yaxis.same=TRUE,
                      yaxis.left=TRUE,
                      yaxis.right=TRUE,
                      grid.ticks.on="months",
                      grid.ticks.lwd=1,
                      grid.ticks.lty=1,
                      grid.col="darkgray",
                      labels.col="#333333",
                      format.labels=TRUE,
                      shading=1,
                      bg.col="#FFFFFF",
                      grid2="#F5F5F5",
                      legend.loc=NULL){
  
  # Small multiples with multiple pages behavior occurs when multi.panel is
  # an integer. (i.e. multi.panel=2 means to iterate over the data in a step
  # size of 2 and plot 2 panels on each page
  # Make recursive calls and return
  if(is.numeric(multi.panel)){
    multi.panel <- min(NCOL(x), multi.panel)
    idx <- seq.int(1L, NCOL(x), 1L)
    chunks <- split(idx, ceiling(seq_along(idx)/multi.panel))
    
    if(!is.null(panels) && nchar(panels) > 0){
      # we will plot the panels, but not plot the returns by column
      multi.panel <- FALSE
    } else {
      # we will plot the returns by column, but not the panels
      multi.panel <- TRUE
      panels <- NULL
      
      if(yaxis.same){
        # If we want the same y-axis and a FUN is specified, we need to
        # apply the transformation first to compute the range for the y-axis
        if(!is.null(FUN) && nchar(FUN) > 0){
          fun <- match.fun(FUN)
          .formals <- formals(fun)
          .formals <- modify.args(formals=.formals, arglist=list(...), dots=TRUE)
          if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=x, dots=TRUE)
          .formals$... <- NULL
          R <- try(do.call(fun, .formals), silent=TRUE)
          if(inherits(R, "try-error")) { 
            message(paste("FUN function failed with message", R))
            ylim <- range(x[subset], na.rm=TRUE)
          } else {
            ylim <- range(R[subset], na.rm=TRUE)
          }
        } else {
           # set the ylim based on the data passed into the x argument
          ylim <- range(x[subset], na.rm=TRUE)
        }
      }
    }
    
    for(i in 1:length(chunks)){
      tmp <- chunks[[i]]
      p <- plot.xts(x=x[,tmp], 
                     y=y,
                     ...=...,
                     subset=subset,
                     FUN=FUN,
                     panels=panels,
                     multi.panel=multi.panel,
                     colorset=colorset,
                     up.col=up.col,
                     dn.col=dn.col,
                     type=type,
                     lty=lty,
                     lwd=lwd,
                     lend=lend,
                     main=main,  
                     clev=clev,
                     cex=cex, 
                     cex.axis=cex.axis,
                     mar=mar, 
                     srt=srt,
                     xaxis.las=xaxis.las,
                     ylim=ylim,
                     yaxis.same=yaxis.same,
                     yaxis.left=yaxis.left,
                     yaxis.right=yaxis.right,
                     grid.ticks.on=grid.ticks.on,
                     grid.ticks.lwd=grid.ticks.lwd,
                     grid.ticks.lty=grid.ticks.lty,
                     grid.col=grid.col,
                     labels.col=labels.col,
                     format.labels=format.labels,
                     shading=shading,
                     bg.col=bg.col,
                     grid2=grid2,
                     legend.loc=legend.loc)
      if(i < length(chunks))
        print(p)
    }
    # NOTE: return here so we don't draw another chart
    return(p)
  }
  
  cs <- new.replot_xts()
  #cex <- pars$cex
  #mar <- pars$mar
  #line.col <- theme$col$line.col
  #up.col <- theme$col$up.col
  #dn.col <- theme$col$dn.col
  #up.border <- theme$col$up.border
  #dn.border <- theme$col$dn.border
  #format.labels <- theme$format.labels
  if(is.null(grid.ticks.on)) {
    xs <- x[subset]
    major.grid <- c(years=nyears(xs),
                    months=nmonths(xs),
                    days=ndays(xs))
    grid.ticks.on <- names(major.grid)[rev(which(major.grid < 30))[1]]
  } #else grid.ticks.on <- theme$grid.ticks.on
  #label.bg <- theme$col$label.bg
  
  # define a subset function
  cs$subset <- function(x) {
    if(FALSE) {set_ylim <- get_ylim <- set_xlim <- Env <-function(){} }  # appease R parser?
    if(missing(x)) {
      x <- "" #1:NROW(Env$xdata)
    }
    Env$xsubset <<- x
    # set_xlim(c(1,NROW(Env$xdata[Env$xsubset])))
    # non equally spaced x-axis
    set_xlim(range(Env$xycoords$x, na.rm=TRUE))
    ylim <- get_ylim()
    for(y in seq(2,length(ylim),by=2)) {
      if(!attr(ylim[[y]],'fixed'))
        ylim[[y]] <- structure(c(Inf,-Inf),fixed=FALSE)
    }
    lapply(Env$actions,
           function(x) {
             frame <- abs(attr(x, "frame"))
             fixed <- attr(ylim[[frame]],'fixed')
             #fixed <- attr(x, "fixed")
             if(frame %% 2 == 0 && !fixed) {
               lenv <- attr(x,"env")
               if(is.list(lenv)) lenv <- lenv[[1]]
               min.tmp <- min(ylim[[frame]][1],range(lenv$xdata[Env$xsubset], na.rm=TRUE)[1],na.rm=TRUE)
               max.tmp <- max(ylim[[frame]][2],range(lenv$xdata[Env$xsubset], na.rm=TRUE)[2],na.rm=TRUE)
               ylim[[frame]] <<- structure(c(min.tmp,max.tmp),fixed=fixed)
             }
           })
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv, lenv
    set_ylim(ylim)
  }
  environment(cs$subset) <- environment(cs$get_asp)
  
  # add theme and charting parameters to Env
  if(multi.panel){
    cs$set_asp(NCOL(x))
  } else {
    cs$set_asp(3)
  }
  cs$Env$cex <- cex
  cs$Env$mar <- mar
  cs$Env$clev = min(clev+0.01,1) # (0,1]
  #cs$Env$theme$bbands <- theme$bbands
  cs$Env$theme$shading <- shading
  #cs$Env$theme$line.col <- theme$col$line.col
  cs$Env$theme$up.col <- up.col
  cs$Env$theme$dn.col <- dn.col
  #cs$Env$theme$up.border <- up.border
  #cs$Env$theme$dn.border <- dn.border
  cs$Env$theme$colorset <- colorset
  cs$Env$theme$rylab <- yaxis.right
  cs$Env$theme$lylab <- yaxis.left
  cs$Env$theme$bg <- bg.col
  cs$Env$theme$grid <- grid.col
  cs$Env$theme$grid2 <- grid2
  cs$Env$theme$labels <- labels.col
  cs$Env$theme$srt <- srt
  cs$Env$theme$xaxis.las <- xaxis.las
  cs$Env$theme$cex.axis <- cex.axis
  #cs$Env$theme$label.bg <- label.bg
  #cs$Env$theme$coarse.time <- coarse.time
  cs$Env$format.labels <- format.labels
  cs$Env$grid.ticks.on <- grid.ticks.on
  cs$Env$grid.ticks.lwd <- grid.ticks.lwd
  cs$Env$grid.ticks.lty <- grid.ticks.lty
  cs$Env$type <- type
  cs$Env$lty <- lty
  cs$Env$lwd <- lwd
  cs$Env$lend <- lend
  cs$Env$legend.loc <- legend.loc
  cs$Env$call_list <- list()
  cs$Env$call_list[[1]] <- match.call()
  
  # Do some checks on x
  if(is.character(x))
    stop("'x' must be a time-series object")
  
  # If we detect an OHLC object, we should call quantmod::chart_Series
  #if(is.OHLC(x)) {
  #  cs$Env$xdata <- OHLC(x)
  #  if(has.Vo(x))
  #    cs$Env$vo <- Vo(x)
  #} else 
  
  # Raw returns data passed into function
  cs$Env$xdata <- x
  cs$Env$xsubset <- subset
  cs$Env$column_names <- colnames(x)
  cs$Env$nobs <- NROW(cs$Env$xdata)
  cs$Env$main <- main
  
  # non equally spaced x-axis
  xycoords <- xy.coords(.index(cs$Env$xdata[cs$Env$xsubset]), 
                        cs$Env$xdata[cs$Env$xsubset][,1])
  cs$Env$xycoords <- xycoords
  cs$Env$xlim <- range(xycoords$x, na.rm=TRUE)
  cs$Env$xstep <- diff(xycoords$x[1:2])
  
  # Compute transformation if specified by panel argument
  # rough prototype for calling a function for the main "panel"
  if(!is.null(FUN)){
    fun <- match.fun(FUN)
    .formals <- formals(fun)
    .formals <- modify.args(formals=.formals, arglist=list(...), dots=TRUE)
    if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=x, dots=TRUE)
    if("x" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, x=x, dots=TRUE)
    .formals$... <- NULL
    R <- try(do.call(fun, .formals), silent=TRUE)
    if(inherits(R, "try-error")) { 
      message(paste("FUN function failed with message", R))
      cs$Env$R <- x
    } else {
      cs$Env$R <- R
    }
  } else {
    cs$Env$R <- x
  }
  
  # Set xlim based on the raw returns data passed into function
  # cs$set_xlim(c(1,NROW(cs$Env$xdata[subset])))
  # non equally spaced x-axis
  cs$set_xlim(cs$Env$xlim)
  
  
  # Set ylim based on the transformed data
  # chart_Series uses fixed=FALSE and add_* uses fixed=TRUE, not sure why or
  # which is best.
  if(is.null(ylim)){
    if(isTRUE(multi.panel)){
      if(yaxis.same){
        # set the ylim for the first panel based on all the data
        cs$set_ylim(list(structure(range(cs$Env$R[subset], na.rm=TRUE),fixed=TRUE)))
      } else {
        # set the ylim for the first panel based on the first column
        cs$set_ylim(list(structure(range(cs$Env$R[,1][subset], na.rm=TRUE),fixed=TRUE))) 
      }
    } else {
      # set the ylim based on all the data if this is not a multi.panel plot
      cs$set_ylim(list(structure(range(cs$Env$R[subset], na.rm=TRUE),fixed=TRUE)))
    }
    cs$Env$constant_ylim <- range(cs$Env$R[subset], na.rm=TRUE)
  } else {
    # use the ylim arg passed in
    cs$set_ylim(list(structure(ylim, fixed=TRUE)))
    cs$Env$constant_ylim <- ylim
  }
  
  cs$set_frame(1,FALSE)
  # axis_ticks function to label lower frequency ranges/grid lines
  #cs$Env$axis_ticks <- function(xdata,xsubset) {
  #  ticks <- diff(axTicksByTime2(xdata[xsubset],labels=FALSE))/2 + 
  #    last(axTicksByTime2(xdata[xsubset],labels=TRUE),-1)
  #  if(min(diff(ticks)) < max(strwidth(names(ticks)))) {
  #    ticks <- unname(ticks)
  #  }
  #  ticks
  #}
  
  # compute the x-axis ticks
  cs$add(expression(atbt <- axTicksByTime2(xdata[xsubset]),
                    segments(xycoords$x[atbt], #axTicksByTime2(xdata[xsubset]),
                             get_ylim()[[2]][1],
                             xycoords$x[atbt], #axTicksByTime2(xdata[xsubset]),
                             get_ylim()[[2]][2], 
                             col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)),
         clip=FALSE,expr=TRUE)
  
  # Add frame for the chart "header" to display the name and start/end dates
  cs$add_frame(0,ylim=c(0,1),asp=0.5)
  cs$set_frame(1)
  
  # add observation level ticks on x-axis if < 400 obs.
  cs$add(expression(if(NROW(xdata[xsubset])<400) 
  {axis(1,at=xycoords$x,labels=FALSE,col=theme$grid2,tcl=0.3)}),expr=TRUE)
  
  # add "month" or "month.abb"
  cs$add(expression(axt <- axTicksByTime(xdata[xsubset],format.labels=format.labels),
                    axis(1,
                         at=xycoords$x[axt], #axTicksByTime(xdata[xsubset]),
                         labels=names(axt), #axTicksByTime(xdata[xsubset],format.labels=format.labels)),
                         las=theme$xaxis.las, lwd.ticks=1, mgp=c(3,1.5,0), 
                         tcl=-0.4, cex.axis=theme$cex.axis)),
         expr=TRUE)
  
  # add main and start/end dates
  #if((isTRUE(multi.panel)) | (multi.panel == 1) | (NCOL(x) == 1))
  #  cs$Env$main <- cs$Env$column_names[1] else cs$Env$main <- main
  
  text.exp <- c(expression(text(xlim[1],0.5,main,font=2,col='#444444',offset=0,cex=1.1,pos=4)),
                expression(text(xlim[2],0.5,
                                paste(start(xdata[xsubset]),end(xdata[xsubset]),sep=" / "),
                                col=1,adj=c(0,0),pos=2)))
  cs$add(text.exp, env=cs$Env, expr=TRUE)
  
  cs$set_frame(2)
  # define function for y-axis labels
  #cs$Env$grid_lines <- function(xdata, xsubset) {
  #  ylim <- range(xdata[xsubset])
  #  p <- pretty(ylim, 5)
  #  p[p > ylim[1] & p < ylim[2]]
  #}
  
  cs$Env$y_grid_lines <- function(ylim) { 
    #pretty(range(xdata[xsubset]))
    p <- pretty(ylim,5)
    p[p > ylim[1] & p < ylim[2]]
  }
  
  # add y-axis grid lines and labels
  exp <- expression(segments(xlim[1], 
                             y_grid_lines(get_ylim()[[2]]), 
                             xlim[2], 
                             y_grid_lines(get_ylim()[[2]]), 
                             col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty))
  if(yaxis.left){
    exp <- c(exp, 
             # left y-axis labels
             expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(get_ylim()[[2]]))), 
                             y_grid_lines(get_ylim()[[2]]),
                             noquote(format(y_grid_lines(get_ylim()[[2]]), justify="right")),
                             col=theme$labels, srt=theme$srt, offset=0, pos=4, 
                             cex=theme$cex.axis, xpd=TRUE)))
  }
  if(yaxis.right){
    exp <- c(exp, 
             # right y-axis labels
             expression(text(xlim[2]+xstep*2/3,
                             y_grid_lines(get_ylim()[[2]]),
                             noquote(format(y_grid_lines(get_ylim()[[2]]), justify="right")),
                             col=theme$labels, srt=theme$srt, offset=0, pos=4, 
                             cex=theme$cex.axis, xpd=TRUE)))
  }
  cs$add(exp, env=cs$Env, expr=TRUE)
  
  # add main series
  cs$set_frame(2)
  if(isTRUE(multi.panel)){
    # We need to plot the first "panel" here because the plot area is
    # set up based on the code above
    lenv <- new.env()
    lenv$xdata <- cs$Env$R[,1][subset]
    lenv$label <- colnames(cs$Env$R[,1])
    lenv$type <- cs$Env$type
    if(yaxis.same){
      lenv$ylim <- cs$Env$constant_ylim
    } else {
      lenv$ylim <- range(cs$Env$R[,1][subset], na.rm=TRUE)
    }
    exp <- expression(chart.lines(xdata, 
                                  type=type, 
                                  lty=lty,
                                  lwd=lwd,
                                  lend=lend,
                                  colorset=theme$colorset, 
                                  up.col=theme$up.col, 
                                  dn.col=theme$dn.col,
                                  legend.loc=legend.loc))
    # Add expression for the main plot
    cs$add(exp, env=c(lenv,cs$Env), expr=TRUE)
    text.exp <- expression(text(x=xycoords$x[2],
                                y=ylim[2]*0.9,
                                labels=label,
                                adj=c(0,0),cex=1,offset=0,pos=4))
    cs$add(text.exp,env=c(lenv, cs$Env),expr=TRUE)
    
    if(NCOL(cs$Env$xdata) > 1){
      for(i in 2:NCOL(cs$Env$xdata)){
        # create a local environment
        lenv <- new.env()
        lenv$xdata <- cs$Env$R[,i][subset]
        lenv$label <- cs$Env$column_names[i]
        if(yaxis.same){
          lenv$ylim <- cs$Env$constant_ylim
        } else {
          lenv$ylim <- range(cs$Env$R[,i][subset], na.rm=TRUE)
        }
        lenv$type <- cs$Env$type
        
        # Add a small frame
        cs$add_frame(ylim=c(0,1),asp=0.25)
        cs$next_frame()
        text.exp <- expression(text(x=xlim[1],
                                    y=0.5,
                                    labels="",
                                    adj=c(0,0),cex=0.9,offset=0,pos=4))
        cs$add(text.exp, env=c(lenv,cs$Env), expr=TRUE)
        
        # Add the frame for the sub-plots
        # Set the ylim based on the (potentially) transformed data in cs$Env$R
        cs$add_frame(ylim=lenv$ylim, asp=NCOL(cs$Env$xdata), fixed=TRUE)
        cs$next_frame()
        
        exp <- expression(chart.lines(xdata[xsubset], 
                                      type=type, 
                                      lty=lty,
                                      lwd=lwd,
                                      lend=lend,
                                      colorset=theme$colorset, 
                                      up.col=theme$up.col, 
                                      dn.col=theme$dn.col,
                                      legend.loc=legend.loc))
        
        # define function to plot the y-axis grid lines
        lenv$y_grid_lines <- function(ylim) { 
          #pretty(range(xdata[xsubset]))
          p <- pretty(ylim,5)
          p[p > ylim[1] & p < ylim[2]]
        }
        
        # NOTE 'exp' was defined earlier as chart.lines
        exp <- c(exp, 
                 # y-axis grid lines
                 expression(segments(xlim[1],
                                     y_grid_lines(ylim),
                                     xlim[2], 
                                     y_grid_lines(ylim), 
                                     col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)),
                 # x-axis grid lines
                 expression(atbt <- axTicksByTime2(xdata[xsubset]),
                            segments(xycoords$x[atbt], #axTicksByTime2(xdata[xsubset]),
                                     ylim[1],
                                     xycoords$x[atbt], #axTicksByTime2(xdata[xsubset]),
                                     ylim[2], 
                                     col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)))
        if(yaxis.left){
          exp <- c(exp, 
                   # y-axis labels/boxes
                   expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(ylim))), 
                                   y_grid_lines(ylim),
                                   noquote(format(y_grid_lines(ylim),justify="right")),
                                   col=theme$labels, srt=theme$srt, offset=0, 
                                   pos=4, cex=theme$cex.axis, xpd=TRUE)))
        }
        if(yaxis.right){
          exp <- c(exp, 
                   expression(text(xlim[2]+xstep*2/3, y_grid_lines(ylim),
                                   noquote(format(y_grid_lines(ylim),justify="right")),
                                   col=theme$labels, srt=theme$srt, offset=0,
                                   pos=4, cex=theme$cex.axis, xpd=TRUE)))
        }
        cs$add(exp,env=c(lenv, cs$Env),expr=TRUE,no.update=TRUE)
        text.exp <- expression(text(x=xycoords$x[2],
                                    y=ylim[2]*0.9,
                                    labels=label,
                                    adj=c(0,0),cex=1,offset=0,pos=4))
        cs$add(text.exp,env=c(lenv, cs$Env),expr=TRUE)
      }
  }
  } else {
    if(type == "h" & NCOL(x) > 1) 
      warning("only the univariate series will be plotted")
    cs$add(expression(chart.lines(R[xsubset], 
                                  type=type, 
                                  lty=lty,
                                  lwd=lwd,
                                  lend=lend,
                                  colorset=theme$colorset,
                                  up.col=theme$up.col, 
                                  dn.col=theme$dn.col,
                                  legend.loc=legend.loc)),expr=TRUE)
    assign(".xts_chob", cs, .plotxtsEnv)
  }
  
  # Plot the panels or default to a simple line chart
  if(!is.null(panels) && nchar(panels) > 0) {
    panels <- parse(text=panels, srcfile=NULL)
    for( p in 1:length(panels)) {
      if(length(panels[p][[1]][-1]) > 0) {
        cs <- eval(panels[p])
      } else {
        cs <- eval(panels[p])
      }
    }
  }

  assign(".xts_chob", cs, .plotxtsEnv)
  cs
} #}}}

addDrawdowns <- function(geometric=TRUE, ylim=NULL, ...){
  lenv <- new.env()
  lenv$main <- "Drawdowns"
  lenv$plot_drawdowns <- function(x, geometric, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    colorset <- x$Env$theme$colorset
    # Add x-axis grid lines
    atbt <- axTicksByTime2(xdata[xsubset])
    segments(x$Env$xycoords$x[atbt],
             par("usr")[3],
             x$Env$xycoords$x[atbt],
             par("usr")[4],
             col=x$Env$theme$grid)
    drawdowns <- PerformanceAnalytics:::Drawdowns(xdata, geometric)[xsubset]
    chart.lines(drawdowns, type="l", colorset=colorset) 
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(geometric=geometric,...)),
         list(geometric=geometric,...))
  exp <- parse(text=gsub("list","plot_drawdowns",
                         as.expression(substitute(list(x=current.xts_chob(),
                                                       geometric=geometric,...)))),
               srcfile=NULL)
  
  plot_object <- current.xts_chob()
  ncalls <- length(plot_object$Env$call_list)
  plot_object$Env$call_list[[ncalls+1]] <- match.call()
  
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  
  drawdowns <- PerformanceAnalytics:::Drawdowns(plot_object$Env$xdata, geometric=geometric)
  lenv$xdata <- drawdowns
  
  # add the frame for drawdowns info
  plot_object$add_frame(ylim=c(0,1),asp=0.25)
  plot_object$next_frame()
  text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                              col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
  
  # add frame for the actual drawdowns data
  if(is.null(ylim)) {
    ylim <- range(lenv$xdata[xsubset], na.rm=TRUE)
    lenv$ylim <- ylim
  }
  plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
  plot_object$next_frame()
  
  lenv$grid_lines <- function(ylim) {
    #ylim <- range(xdata[xsubset])
    p <- pretty(ylim, 5)
    p[p > ylim[1] & p < ylim[2]]
  }
  # add y-axis gridlines and labels
  exp <- c(expression(segments(xlim[1],
                               grid_lines(ylim),
                               xlim[2],
                               grid_lines(ylim),
                               col=theme$grid)), 
           exp,  # NOTE 'exp' was defined earlier
           # add axis labels/boxes
           expression(text(xlim[1]-xstep*2/3-max(strwidth(grid_lines(ylim))),
                           grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)),
           expression(text(xlim[2]+xstep*2/3,
                           grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  plot_object
}

#' Add a time series to an existing xts plot
#' 
#' @param x an xts object to plot.
#' @param main main title for a new panel if drawn.
#' @param on panel number to draw on. A new panel will be drawn if \code{on=NA}.
#' @param type the type of plot to be drawn, same as in \code{\link{plot}}.
#' @param col color palette to use, set by default to rational choices.
#' @param lty set the line type, same as in \code{\link{plot}}.
#' @param lwd set the line width, same as in \code{\link{plot}}.
#' @param pch the type of plot to be drawn, same as in \code{\link{plot}}.
#' @param \dots any other passthrough parameters. Not currently used.
#' @author Ross Bennett
addSeries <- function(x, main="", on=NA, type="l", col=NULL, lty=1, lwd=1, pch=0, ...){
  lenv <- new.env()
  lenv$main <- main
  lenv$plot_lines <- function(x, ta, on, type, col, lty, lwd, pch, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    if(is.null(col)){
      colorset <- x$Env$theme$colorset
    } else {
      colorset <- col
    }
    if(all(is.na(on))){
      # Add x-axis grid lines
      atbt <- axTicksByTime2(xdata[xsubset])
      segments(x$Env$xycoords$x[atbt],
               par("usr")[3],
               x$Env$xycoords$x[atbt],
               par("usr")[4],
               col=x$Env$theme$grid)
    }
    # we can add points that are not necessarily at the points
    # on the main series
    subset.range <- paste(start(xdata[xsubset]),
                          end(xdata[xsubset]),sep="/")
    ta.adj <- merge(n=.xts(1:NROW(xdata[xsubset]),
                           .index(xdata[xsubset]), 
                           tzone=indexTZ(xdata)),ta)[subset.range]
    ta.x <- as.numeric(na.approx(ta.adj[,1], rule=2) )
    ta.y <- ta.adj[,-1]
    chart.lines(ta.y, type=type, colorset=colorset, lty=lty, lwd=lwd, pch=pch)
  }
  # map all passed args (if any) to 'lenv' environment
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(x=x,on=on,type=type,col=col,lty=lty,lwd=lwd,pch=pch,...)),
         list(x=x,on=on,type=type,col=col,lty=lty,lwd=lwd,pch=pch,...))
  exp <- parse(text=gsub("list","plot_lines",
                         as.expression(substitute(list(x=current.xts_chob(),
                                                       ta=get("x"),
                                                       on=on,
                                                       type=type,
                                                       col=col,
                                                       lty=lty,
                                                       lwd=lwd,
                                                       pch=pch,
                                                       ...)))),
               srcfile=NULL)
  
  plot_object <- current.xts_chob()
  ncalls <- length(plot_object$Env$call_list)
  plot_object$Env$call_list[[ncalls+1]] <- match.call()
  
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  no.update <- FALSE
  lenv$xdata <- merge(x,xdata,retside=c(TRUE,FALSE))
  ylim <- range(lenv$xdata[xsubset], na.rm=TRUE)
  lenv$ylim <- ylim
  
  if(is.na(on)){
    # add the frame for drawdowns info
    plot_object$add_frame(ylim=c(0,1),asp=0.25)
    plot_object$next_frame()
    text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                                col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
    
    # add frame for the data
    plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
    plot_object$next_frame()
    
    # define function to plot the y-axis grid lines
    lenv$y_grid_lines <- function(ylim) { 
      #pretty(range(xdata[xsubset]))
      p <- pretty(ylim,5)
      p[p > ylim[1] & p < ylim[2]]
    }
    
    # NOTE 'exp' was defined earlier as chart.lines
    exp <- c(exp, 
             # y-axis grid lines
             expression(segments(xlim[1],
                                 y_grid_lines(ylim),
                                 xlim[2], 
                                 y_grid_lines(ylim), 
                                 col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)))
    if(plot_object$Env$theme$lylab){
      exp <- c(exp, 
               # y-axis labels/boxes
               expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(ylim))), 
                               y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=0, 
                               pos=4, cex=theme$cex.axis, xpd=TRUE)))
    }
    if(plot_object$Env$theme$rylab){
      exp <- c(exp, 
               expression(text(xlim[2]+xstep*2/3, 
                               y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=0,
                               pos=4, cex=theme$cex.axis, xpd=TRUE)))
    }
    plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  } else {
    for(i in 1:length(on)) {
      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
    }
  }
  plot_object
}

#' Add time series of points to an existing xts plot
#' 
#' @param x an xts object to plot.
#' @param main main title for a new panel if drawn.
#' @param on panel number to draw on. A new panel will be drawn if \code{on=NA}.
#' @param col color palette to use, set by default to rational choices.
#' @param pch the type of plot to be drawn, same as in \code{\link{plot}}.
#' @param \dots any other passthrough parameters. Not currently used.
#' @author Ross Bennett
addPoints <- function(x, main="", on=NA, col=NULL, pch=0, ...){
  addSeries(x, main=main, on=on, type="p", col=col, pch=pch, ...)
}

#' Add vertical lines to an existing xts plot
#' 
#' @param event.lines character vector of dates. Vertical lines will be drawn 
#' to indicate that an event happened during that time period.  \code{event.lines} should
#' be a vector of dates (e.g., \code{c("09/03","05/06"))} formatted the same as
#' \code{date.format}. This function matches the re-formatted row names (dates) with
#' the events.list, so to get a match the formatting needs to be correct.
#' @param event.labels character vector of event labels corresponding to 
#' \code{event.lines}. This will apply text labels (e.g., 
#' \code{c("This Event", "That Event")} to the vertical lines drawn.
#' @param date.format format for the dates in \code{event.lines}.
#' @param main main title for a new panel if drawn.
#' @param on panel number to draw on. A new panel will be drawn if \code{on=NA}.
#' @param lty set the line type, same as in \code{\link{plot}}.
#' @param lwd set the line width, same as in \code{\link{plot}}.
#' @param col color palette to use, set by default to rational choices.
#' @param \dots any other passthrough parameters. Not currently used.
#' @author Ross Bennett
addLines <- function(event.dates, event.labels=NULL, date.format="%Y-%m-%d", main="", on=NA, lty=1, lwd=1, col=1, ...){
  # add checks for event.dates and event.labels
  if(!is.null(event.labels))
    if(length(event.dates) != length(event.labels)) stop("length of event.dates must match length of event.labels")
  
  lenv <- new.env()
  lenv$main <- main
  lenv$plot_event_lines <- function(x, event.dates, event.labels, date.format, on, lty, lwd, col, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    colorset <- x$Env$theme$colorset
    if(all(is.na(on))){
      # Add x-axis grid lines
      atbt <- axTicksByTime2(xdata[xsubset])
      segments(x$Env$xycoords$x[atbt],
               par("usr")[3],
               x$Env$xycoords$x[atbt],
               par("usr")[4],
               col=x$Env$theme$grid)
    }
    ypos <- x$Env$ylim[[2*on]][2]
    # create a new xts object out of event.dates
    event.dates.xts <- xts(rep(999, length(event.dates)), order.by=as.Date(event.dates, format=date.format))
    # we can add points that are not necessarily at the points on the main series
    subset.range <- paste(start(xdata[xsubset]),
                          end(xdata[xsubset]),sep="/")
    ta.adj <- merge(n=.xts(1:NROW(xdata[xsubset]),
                           .index(xdata[xsubset]), 
                           tzone=indexTZ(xdata)),event.dates.xts)[subset.range]
    ta.x <- as.numeric(na.approx(ta.adj[,1], rule=2) )
    ta.y <- ta.adj[,-1]
    event.ind <- which(ta.y == 999)
    abline(v=x$Env$xycoords$x[event.ind], col=col, lty=lty, lwd=lwd)
    text(x=x$Env$xycoords$x[event.ind], y=ypos, labels=event.labels, offset=.2, pos=2, , srt=90, col=1)
  }
  
  plot_object <- current.xts_chob()
  ncalls <- length(plot_object$Env$call_list)
  plot_object$Env$call_list[[ncalls+1]] <- match.call()
  
  if(is.na(on[1])){
    # map all passed args (if any) to 'lenv' environment
    mapply(function(name,value) { assign(name,value,envir=lenv) }, 
           names(list(event.dates=event.dates,event.labels=event.labels,date.format=date.format,on=on,lty=lty,lwd=lwd,col=col,...)),
           list(event.dates=event.dates,event.labels=event.labels,date.format=date.format,on=on,lty=lty,lwd=lwd,col=col,...))
    exp <- parse(text=gsub("list","plot_event_lines",
                           as.expression(substitute(list(x=current.xts_chob(),
                                                         event.dates=event.dates,
                                                         event.labels=event.labels,
                                                         date.format=date.format,
                                                         on=on,
                                                         lty=lty,
                                                         lwd=lwd,
                                                         col=col,
                                                         ...)))),
                 srcfile=NULL)
    
    xdata <- plot_object$Env$xdata
    xsubset <- plot_object$Env$xsubset
    no.update <- FALSE
    lenv$xdata <- xdata
    ylim <- range(xdata[xsubset], na.rm=TRUE)
    lenv$ylim <- ylim
  
    # add the frame for drawdowns info
    plot_object$add_frame(ylim=c(0,1),asp=0.25)
    plot_object$next_frame()
    text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                                col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
    
    # add frame for the data
    plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
    plot_object$next_frame()
    
    # define function to plot the y-axis grid lines
    lenv$y_grid_lines <- function(ylim) { 
      #pretty(range(xdata[xsubset]))
      p <- pretty(ylim,5)
      p[p > ylim[1] & p < ylim[2]]
    }
    
    # NOTE 'exp' was defined earlier as chart.lines
    exp <- c(exp, 
             # y-axis grid lines
             expression(segments(xlim[1],
                                 y_grid_lines(ylim),
                                 xlim[2], 
                                 y_grid_lines(ylim), 
                                 col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)))
    if(plot_object$Env$theme$lylab){
      exp <- c(exp, 
               # y-axis labels/boxes
               expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(ylim))), 
                               y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=0, 
                               pos=4, cex=theme$cex.axis, xpd=TRUE)))
    }
    if(plot_object$Env$theme$rylab){
      exp <- c(exp, 
               expression(text(xlim[2]+xstep*2/3, 
                               y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=0,
                               pos=4, cex=theme$cex.axis, xpd=TRUE)))
    }
    plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  } else {
    for(i in 1:length(on)) {
      ind <- on[i]
      no.update <- FALSE
      # map all passed args (if any) to 'lenv' environment
      mapply(function(name,value) { assign(name,value,envir=lenv) }, 
             names(list(event.dates=event.dates,event.labels=event.labels,date.format=date.format,on=ind,lty=lty,lwd=lwd,col=col,...)),
             list(event.dates=event.dates,event.labels=event.labels,date.format=date.format,on=ind,lty=lty,lwd=lwd,col=col,...))
      exp <- parse(text=gsub("list","plot_event_lines",
                             as.expression(substitute(list(x=current.xts_chob(),
                                                           event.dates=event.dates,
                                                           event.labels=event.labels,
                                                           date.format=date.format,
                                                           on=ind,
                                                           lty=lty,
                                                           lwd=lwd,
                                                           col=col,
                                                           ...)))),
                   srcfile=NULL)
  
      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
    }
  }
  plot_object
}


# # Needed for finding aligned dates for event lines and period areas
# rownames = as.Date(time(y))
# rownames = format(strptime(rownames,format = date.format.in), date.format)
# # Add event.lines before drawing the data
# # This only labels the dates it finds
# if(!is.null(event.lines)) {
#   event.ind = NULL
#   for(event in 1:length(event.lines)){
#     event.ind = c(event.ind, grep(event.lines[event], rownames))
#   }
#   number.event.labels = ((length(event.labels)-length(event.ind) + 1):length(event.labels))
#   
#   abline(v = event.ind, col = event.color, lty = 2)
#   if(!is.null(event.labels)) {
#     text(x=event.ind,y=ylim[2], label = event.labels[number.event.labels], offset = .2, pos = 2, cex = cex.labels, srt=90, col = event.color)
#   }
# }



# based on quantmod::add_TA
# addLines <- function(x, main="", order=NULL, on=NA, legend="auto",
#                      yaxis=list(NULL,NULL),
#                      col=1, type="l", ...) { 
#   lenv <- new.env()
#   lenv$main <- main
#   lenv$plot_ta <- function(x, ta, on, type, col,...) {
#     xdata <- x$Env$xdata
#     xsubset <- x$Env$xsubset
#     if(all(is.na(on))) {
#       # x-axis grid lines based on Env$xdata and Env$xsubset
#       segments(axTicksByTime2(xdata[xsubset]),
#                par("usr")[3],
#                axTicksByTime2(xdata[xsubset]),
#                par("usr")[4],
#                col=x$Env$theme$grid)
#     }
#     if(is.logical(ta)) {
#       ta <- merge(ta, xdata, join="right",retside=c(TRUE,FALSE))[xsubset]
#       shade <- shading(as.logical(ta,drop=FALSE))
#       if(length(shade$start) > 0) # all FALSE cause zero-length results
#         rect(shade$start-1/3, par("usr")[3] ,shade$end+1/3, par("usr")[4], col=col,...) 
#     } else {
#       # we can add points that are not necessarily at the points
#       # on the main series
#       subset.range <- paste(start(x$Env$xdata[x$Env$xsubset]),
#                             end(x$Env$xdata[x$Env$xsubset]),sep="/")
#       ta.adj <- merge(n=.xts(1:NROW(x$Env$xdata[x$Env$xsubset]),
#                              .index(x$Env$xdata[x$Env$xsubset]), tzone=indexTZ(x$Env$xdata)),ta)[subset.range]
#       ta.x <- as.numeric(na.approx(ta.adj[,1], rule=2) )
#       ta.y <- ta.adj[,-1]
#       chart.lines(ta.y, colorset=col, type=type)
#     }
#   }
#   lenv$xdata <- x
#   # map all passed args (if any) to 'lenv' environment
#   mapply(function(name,value) { assign(name,value,envir=lenv) }, 
#          names(list(x=x,order=order,on=on,legend=legend,
#                     type=type,col=col,...)),
#          list(x=x,order=order,on=on,legend=legend,
#               type=type,col=col,...))
#   exp <- parse(text=gsub("list","plot_ta",
#                          as.expression(substitute(list(x=current.xts_chob(),
#                                                        ta=get("x"),on=on,
#                                                        type=type,col=col,...)))),
#                srcfile=NULL)
#   plot_object <- current.xts_chob()
#   ncalls <- length(plot_object$Env$call_list)
#   plot_object$Env$call_list[[ncalls+1]] <- match.call()
#   xdata <- plot_object$Env$xdata
#   xsubset <- plot_object$Env$xsubset
#   # if(is.logical(x)) no.update <- TRUE else no.update <- FALSE
#   no.update <- TRUE
#   #  this merge isn't going to work if x isn't in xdata range. Something like:
#   #    na.approx(merge(n=.xts(1:NROW(xdata),.index(xdata)),ta)[,1])
#   #  should allow for any time not in the original to be merged in.
#   #  probably need to subset xdata _before_ merging, else subset will be wrong
#   #
#   #tav <- merge(x, xdata, join="right",retside=c(TRUE,FALSE))
#   #lenv$xdata <- tav
#   #tav <- tav[xsubset]
#   lenv$col <- col
#   lenv$xdata <- merge(x,xdata,retside=c(TRUE,FALSE))
#   
#   if(is.na(on)) {
#     plot_object$add_frame(ylim=c(0,1),asp=0.2)
#     plot_object$next_frame()
#     text.exp <- expression(text(x=1,
#                                 y=0.3,
#                                 labels=main,
#                                 col=c(1,col),adj=c(0,0),cex=0.9,offset=0,pos=4))
#     plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
#     
#     plot_object$add_frame(ylim=range(na.omit(xdata)),asp=1)  # need to have a value set for ylim
#     plot_object$next_frame()
#     # add grid lines, using custom function for MACD gridlines
#     lenv$grid_lines <- function(xdata,xsubset) { 
#       pretty(range(xdata[xsubset]))
#     }
#     exp <- c(expression(segments(1,grid_lines(xdata,xsubset),NROW(xdata[xsubset]),grid_lines(xdata,xsubset),
#                                  col=theme$grid)), exp,  # NOTE 'exp' was defined earlier to be plot_macd
#              # add axis labels/boxes
#              expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
#                              noquote(format(grid_lines(xdata,xsubset),justify="right")),
#                              col=theme$labels,offset=0,pos=4,cex=0.9,xpd=TRUE)),
#              expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
#                              noquote(format(grid_lines(xdata,xsubset),justify="right")),
#                              col=theme$labels,offset=0,pos=4,cex=0.9,xpd=TRUE)))
#     plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
#   } else { 
#     for(i in 1:length(on)) {
#       plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
#       lenv$grid_lines <- function(xdata,xsubset) { 
#         pretty(range(xdata[xsubset]))
#       }
#       exp <- c(exp,
#                # LHS
#                #expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
#                #           noquote(format(grid_lines(xdata,xsubset),justify="right")),
#                #           col=theme$labels,offset=0,pos=4,cex=0.9)),
#                # RHS
#                expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
#                                noquote(format(grid_lines(xdata,xsubset),justify="right")),
#                                col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
#       #}
#       plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
#     }
#   }
#   plot_object
# } #}}}

addReturns <- function(type="h", main=NULL, ylim=NULL){
  # This just plots the raw returns data
  lenv <- new.env()
  if(is.null(main)) lenv$main <- "Returns" else lenv$main <- main
  lenv$plot_returns <- function(x, type) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    colorset <- x$Env$theme$colorset
    up.col <- x$Env$theme$up.col
    dn.col <- x$Env$theme$dn.col
    # Add x-axis grid lines
    atbt <- axTicksByTime2(xdata[xsubset])
    segments(x$Env$xycoords$x[atbt],
             par("usr")[3],
             x$Env$xycoords$x[atbt],
             par("usr")[4],
             col=x$Env$theme$grid)
    chart.lines(xdata[xsubset], type=type, colorset=colorset, up.col=up.col, dn.col=dn.col)
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(type=type)),
         list(type=type))
  exp <- parse(text=gsub("list","plot_returns",
                         as.expression(substitute(list(x=current.xts_chob(), 
                                                       type=type)))),
               srcfile=NULL)
  
  plot_object <- current.xts_chob()
  ncalls <- length(plot_object$Env$call_list)
  plot_object$Env$call_list[[ncalls+1]] <- match.call()
  
  # get the raw returns data
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  
  if(type == "h" & NCOL(xdata) > 1) 
      warning("only the univariate series will be plotted")
  
  # add data to the local environment
  lenv$xdata <- xdata
  lenv$xsubset <- xsubset
  lenv$col <- col
  lenv$type <- type
  
  # add the frame for time series info
  plot_object$add_frame(ylim=c(0,1),asp=0.25)
  plot_object$next_frame()
  text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                              col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
  
  # add frame for the actual data
  if(is.null(ylim)) {
    ylim <- range(lenv$xdata[xsubset], na.rm=TRUE)
    lenv$ylim <- ylim
  }
  plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
  plot_object$next_frame()
  
  lenv$grid_lines <- function(ylim) {
    #ylim <- range(xdata[xsubset])
    p <- pretty(ylim, 5)
    p[p > ylim[1] & p < ylim[2]]
  }
  # add y-axis gridlines and labels
  exp <- c(expression(segments(xlim[1],
                               grid_lines(ylim),
                               xlim[2],
                               grid_lines(ylim),col=theme$grid)), 
           exp,  # NOTE 'exp' was defined earlier
           # add axis labels/boxes
           expression(text(xlim[1]-xstep*2/3-max(strwidth(grid_lines(ylim))),
                           grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)),
           expression(text(xlim[2]+xstep*2/3,
                           grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  plot_object
}

addRollingPerformance <- function(width=12, FUN="Return.annualized", fill=NA, ylim=NULL, ...){
  lenv <- new.env()
  lenv$main <- paste("Rolling", FUN)
  lenv$plot_performance <- function(x, width, FUN, fill, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    colorset <- x$Env$theme$colorset
    up.col <- x$Env$theme$up.col
    dn.col <- x$Env$theme$dn.col
    # Add x-axis grid lines
    segments(axTicksByTime2(xdata[xsubset]),
             par("usr")[3],
             axTicksByTime2(xdata[xsubset]),
             par("usr")[4],
             col=x$Env$theme$grid)
    rolling_performance <- RollingPerformance(R=xdata, width=width, FUN=FUN, fill=fill, ...=...)
    chart.lines(rolling_performance, type="l", colorset=colorset, up.col=up.col, dn.col=dn.col) 
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(width=width, FUN=FUN, fill=fill, ...)),
         list(width=width, FUN=FUN, fill=fill, ...))
  exp <- parse(text=gsub("list","plot_performance",
                         as.expression(substitute(list(x=current.xts_chob(),
                                                       width=width, FUN=FUN, fill=fill, ...)))),
               srcfile=NULL)
  
  plot_object <- current.xts_chob()
  ncalls <- length(plot_object$Env$call_list)
  plot_object$Env$call_list[[ncalls+1]] <- match.call()
  
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  
  rolling_performance <- RollingPerformance(R=plot_object$Env$xdata, width=width, FUN=FUN, ...=..., fill=fill)
  lenv$xdata <- rolling_performance
  lenv$col <- col
  
  # add the frame for drawdowns info
  plot_object$add_frame(ylim=c(0,1),asp=0.25)
  plot_object$next_frame()
  text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                              adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
  
  # add frame for the actual drawdowns data
  if(is.null(ylim)) {
    ylim <- range(lenv$xdata[xsubset], na.rm=TRUE)
    lenv$ylim <- ylim
  }
  plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
  plot_object$next_frame()
  
  lenv$grid_lines <- function(ylim) {
    #ylim <- range(na.omit(xdata[xsubset]))
    p <- pretty(ylim, 5)
    p[p > ylim[1] & p < ylim[2]]
  }
  # add y-axis gridlines and labels
  exp <- c(expression(segments(xlim[1],
                               grid_lines(ylim),
                               xlim[2],
                               grid_lines(ylim),col=theme$grid)), 
           exp,  # NOTE 'exp' was defined earlier
           # add axis labels/boxes
           expression(text(xlim[1]-xstep*2/3-max(strwidth(grid_lines(ylim))),
                           grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)),
           expression(text(xlim[2]+xstep*2/3,
                           grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  plot_object
}

#' Add Legend
#' 
#' @param legend.loc legend.loc places a legend into one of nine locations on 
#' the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, 
#' right, or center.
#' @param legend.names character vector of names for the legend. If \code{NULL},
#' the column names of the current plot object are used.
#' @param colorset fill colorset for the legend. If \code{NULL},
#' the colorset of the current plot object data is used.
#' @param ncol number of columns for the legend
#' @param \dots any other passthrough parameters. Not currently used.
#' @author Ross Bennett
addLegend <- function(legend.loc="center", legend.names=NULL, colorset=NULL, ncol=1, ...){
  lenv <- new.env()
  lenv$main <- ""
  
  plot_object <- current.xts_chob()
  ncalls <- length(plot_object$Env$call_list)
  plot_object$Env$call_list[[ncalls+1]] <- match.call()
  
  # add the frame for drawdowns info
  plot_object$add_frame(ylim=c(0,1),asp=0.25)
  plot_object$next_frame()
  text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                              col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
  
  # add frame for the legend panel
  plot_object$add_frame(ylim=c(0,1),asp=0.8,fixed=TRUE)
  plot_object$next_frame()
  
  if(!is.null(legend.loc)){
    yrange <- c(0,1)
    nobs <- plot_object$Env$nobs
    chob.xlim <- plot_object$Env$xlim
    switch(legend.loc,
           topleft = {
             xjust <- 0
             yjust <- 1
             lx <- chob.xlim[1]
             ly <- yrange[2]
           },
           left = {
             xjust <- 0
             yjust <- 0.5
             lx <- chob.xlim[1]
             ly <- sum(yrange) / 2
           },
           bottomleft = {
             xjust <- 0
             yjust <- 0
             lx <- chob.xlim[1]
             ly <- yrange[1]
           },
           top = {
             xjust <- 0.5
             yjust <- 1
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- yrange[2]
           },
           center = {
             xjust <- 0.5
             yjust <- 0.5
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- sum(yrange) / 2
           },
           bottom = {
             xjust <- 0.5
             yjust <- 0
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- yrange[1]
           },
           topright = {
             xjust <- 1
             yjust <- 1
             lx <- chob.xlim[2]
             ly <- yrange[2]
           },
           right = {
             xjust <- 1
             yjust <- 0.5
             lx <- chob.xlim[2]
             ly <- sum(yrange) / 2
           },
           bottomright = {
             xjust <- 1
             yjust <- 0
             lx <- chob.xlim[2]
             ly <- yrange[1]
           }
    )
  }
  nc <- NCOL(plot_object$Env$xdata)
  lenv$lx <- lx
  lenv$ly <- ly
  lenv$xjust <- xjust
  lenv$yjust <- yjust
  if(!is.null(colorset)){
    lenv$colorset <- colorset[1:nc]
  } else {
    lenv$colorset <- plot_object$Env$theme$colorset[1:nc]
  }
  if(!is.null(legend.names)){
    lenv$names <- legend.names
  } else {
    lenv$names <- plot_object$Env$column_names
  }
  lenv$nc <- ncol
  # add expression for legend
  exp <- expression(legend(x=lx, y=ly, legend=names, xjust=xjust, yjust=yjust, 
                           fill=colorset, ncol=nc, bty="n"))
  
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  plot_object
}
