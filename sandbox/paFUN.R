# prototypes for functions that will likely make their way into PerformanceAnalytics
addDrawdowns <- function(geometric=TRUE, ylim=NULL, ...){
  lenv <- new.env()
  lenv$main <- "Drawdowns"
  lenv$plot_drawdowns <- function(x, geometric, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    col <- x$Env$theme$col
    # Add x-axis grid lines
    atbt <- xtsExtra:::axTicksByTime2(xdata[xsubset])
    segments(x$Env$xycoords$x[atbt],
             par("usr")[3],
             x$Env$xycoords$x[atbt],
             par("usr")[4],
             col=x$Env$theme$grid)
    drawdowns <- PerformanceAnalytics:::Drawdowns(xdata, geometric)[xsubset]
    xtsExtra:::chart.lines(drawdowns, type="l", col=col) 
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(geometric=geometric,...)),
         list(geometric=geometric,...))
  exp <- parse(text=gsub("list","plot_drawdowns",
                         as.expression(substitute(list(x=xtsExtra:::current.xts_chob(),
                                                       geometric=geometric,...)))),
               srcfile=NULL)
  
  plot_object <- xtsExtra:::current.xts_chob()
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

addReturns <- function(type="h", main=NULL, ylim=NULL){
  # This just plots the raw returns data
  lenv <- new.env()
  if(is.null(main)) lenv$main <- "Returns" else lenv$main <- main
  lenv$plot_returns <- function(x, type) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    col <- x$Env$theme$col
    up.col <- x$Env$theme$up.col
    dn.col <- x$Env$theme$dn.col
    # Add x-axis grid lines
    atbt <- xtsExtra:::axTicksByTime2(xdata[xsubset])
    segments(x$Env$xycoords$x[atbt],
             par("usr")[3],
             x$Env$xycoords$x[atbt],
             par("usr")[4],
             col=x$Env$theme$grid)
    xtsExtra:::chart.lines(xdata[xsubset], type=type, col=col, up.col=up.col, dn.col=dn.col)
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(type=type)),
         list(type=type))
  exp <- parse(text=gsub("list","plot_returns",
                         as.expression(substitute(list(x=xtsExtra:::current.xts_chob(), 
                                                       type=type)))),
               srcfile=NULL)
  
  plot_object <- xtsExtra:::current.xts_chob()
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
    col <- x$Env$theme$col
    up.col <- x$Env$theme$up.col
    dn.col <- x$Env$theme$dn.col
    # Add x-axis grid lines
    segments(xtsExtra:::axTicksByTime2(xdata[xsubset]),
             par("usr")[3],
             xtsExtra:::axTicksByTime2(xdata[xsubset]),
             par("usr")[4],
             col=x$Env$theme$grid)
    rolling_performance <- RollingPerformance(R=xdata, width=width, FUN=FUN, fill=fill, ...=...)
    xtsExtra:::chart.lines(rolling_performance, type="l", col=col, up.col=up.col, dn.col=dn.col) 
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(width=width, FUN=FUN, fill=fill, ...)),
         list(width=width, FUN=FUN, fill=fill, ...))
  exp <- parse(text=gsub("list","plot_performance",
                         as.expression(substitute(list(x=xtsExtra:::current.xts_chob(),
                                                       width=width, FUN=FUN, fill=fill, ...)))),
               srcfile=NULL)
  
  plot_object <- xtsExtra:::current.xts_chob()
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



CumReturns <-
  function (R, wealth.index = FALSE, geometric = TRUE, begin = c("first","axis"))
  { # @author Peter Carl
    
    # DESCRIPTION:
    # Cumulates the returns given and draws a line graph of the results as
    # a cumulative return or a "wealth index".
    
    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # wealth.index:  if true, shows the "value of $1", starting the cumulation
    #    of returns at 1 rather than zero
    # legend.loc: use this to locate the legend, e.g., "topright"
    # colorset: use the name of any of the palattes above
    # method: "none"
    
    # Outputs:
    # A timeseries line chart of the cumulative return series
    
    # FUNCTION:
    
    # Transform input data to a matrix
    begin = begin[1]
    x = checkData(R)
    
    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)
    
    # Calculate the cumulative return
    one = 0
    if(!wealth.index)
      one = 1
    
    ##find the longest column, calc cum returns and use it for starting values
    
    if(begin == "first") {
      length.column.one = length(x[,1])
      # find the row number of the last NA in the first column
      start.row = 1
      start.index = 0
      while(is.na(x[start.row,1])){
        start.row = start.row + 1
      }
      x = x[start.row:length.column.one,]
      if(geometric)
        reference.index = PerformanceAnalytics:::na.skip(x[,1],FUN=function(x) {cumprod(1+x)})
      else
        reference.index = PerformanceAnalytics:::na.skip(x[,1],FUN=function(x) {cumsum(x)})
    }
    for(column in 1:columns) {
      if(begin == "axis") {
        start.index = FALSE
      } else {
        # find the row number of the last NA in the target column
        start.row = 1
        while(is.na(x[start.row,column])){
          start.row = start.row + 1
        }
        start.index=ifelse(start.row > 1,TRUE,FALSE)
      }
      if(start.index){
        # we need to "pin" the beginning of the shorter series to the (start date - 1 period) 
        # value of the reference index while preserving NA's in the shorter series
        if(geometric)
          z = PerformanceAnalytics:::na.skip(x[,column],FUN = function(x,index=reference.index[(start.row - 1)]) {rbind(index,1+x)})
        else
          z = PerformanceAnalytics:::na.skip(x[,column],FUN = function(x,index=reference.index[(start.row - 1)]) {rbind(1+index,1+x)})
      } else {
        z = 1+x[,column] 
      }
      column.Return.cumulative = PerformanceAnalytics:::na.skip(z,FUN = function(x, one, geometric) {if(geometric) cumprod(x)-one else (1-one) + cumsum(x-1)},one=one, geometric=geometric)
      if(column == 1)
        Return.cumulative = column.Return.cumulative
      else
        Return.cumulative = merge(Return.cumulative,column.Return.cumulative)
    }
    if(columns == 1)
      Return.cumulative = as.xts(Return.cumulative)
    colnames(Return.cumulative) = columnnames
    
    return(Return.cumulative)
  }
#addingtest command from performance 
RollingPerformance <- function (R, width = 12, FUN = "Return.annualized", ..., fill = NA)
{ # @author Peter Carl
  
  # DESCRIPTION:
  # A wrapper to create a chart of rolling peRformance metrics in a line chart
  
  # Inputs:
  # R: a matrix, data frame, or timeSeries of returns
  # FUN: any function that can be evaluated using a single set of returns
  #   (e.g., rolling beta won't work, but Return.annualizeds will)
  
  # Outputs:
  # A timeseries line chart of the calculated series
  
  # FUNCTION:
  
  # Transform input data to a matrix
  x = checkData(R)
  
  # Get dimensions and labels
  columns = ncol(x)
  columnnames = colnames(x)
  
  # Separate function args from plot args
  dotargs <-list(...)
  funargsmatch = pmatch(names(dotargs), names(formals(FUN)), nomatch = 0L)
  funargs = dotargs[funargsmatch>0L]
  if(is.null(funargs))funargs=list()
  funargs$...=NULL
  
  funargs$width=width
  funargs$FUN=FUN
  funargs$fill = fill
  funargs$align='right'
  
  # Calculate
  for(column in 1:columns) {
    # the drop=FALSE flag is essential for when the zoo object only has one column
    rollargs<-c(list(data=na.omit(x[,column,drop=FALSE])),funargs)
    column.Return.calc <- do.call(rollapply,rollargs)
    if(column == 1)
      Return.calc = xts(column.Return.calc)
    else
      Return.calc = merge(Return.calc,column.Return.calc)
  }
  colnames(Return.calc) = columnnames
  Return.calc
}
##adding another test
ACFwoo <- function(R, maxlag = NULL, elementcolor = "gray", main = NULL, ...)
{ # @author David Stoffer and Robert Shumway
  # @modifiedby Peter Carl
  
  # DESCRIPTION:
  
  # Inspired by the same charts as chart.ACFplus.R
  
  # From the website: http://www.stat.pitt.edu/stoffer/tsa2/Rcode/acf2.R
  # "...here's an R function that will plot the ACF and PACF of a time series 
  # at the same time on the SAME SCALE, and it leaves out the zero lag in the 
  # ACF: acf2.R. If your time series is in x and you want the ACF and PACF of 
  # x to lag 50, the call to the function is acf2(x,50). The number of lags 
  # is optional, so acf2(x) will use a default number of lags [√n + 10, where 
  # n is the number of observations]."
  
  # This function uses those same defaults to print just the ACF chart.
  
  R = checkData(R)
  data = checkData(R[,1], method="vector", na.rm = TRUE)
  
  columns = ncol(R)
  rows = nrow(R)
  columnnames = colnames(R)
  
  if(is.null(main))
    main = columnnames[1]
  
  num = length(data)
  if (is.null(maxlag)) 
    maxlag = ceiling(10 + sqrt(num))
  ACF = acf(data, maxlag, plot = FALSE)$acf[-1]
  Lag = 1:length(ACF)/frequency(data)
  minA = min(ACF)
  U = 2/sqrt(num)
  L = -U
  minu = min(minA, L) - .01
  
  plot(Lag, ACF, type = "h", ylim = c(minu,1), main = main, axes = FALSE, ...)
  box(col=elementcolor)
  axis(2, col = elementcolor, cex.axis = 0.8)
  axis(1, col = elementcolor, cex.axis = 0.8)
  abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  
}

rollingreg= function (Ra, Rb, width = 12, Rf = 0, main = NULL, legend.loc = NULL, event.labels=NULL, ...)
{ # @author Peter Carl
  
  # DESCRIPTION:
  # A wrapper to create a panel of RollingRegression charts that demonstrates
  # how the attributes change through time.
  
  # Inputs:
  # Ra: a matrix, data frame, or timeSeries, usually a set of monthly returns.
  #   The first column is assumed to be the returns of interest, the next
  #   columns are assumed to be relevant benchmarks for comparison.
  # Rb: a matrix, data frame, or timeSeries that is a set of returns of the
  #   same scale and periodicity as R.
  # Rf: the risk free rate.  Remember to set this to the same periodicity
  #   as the data being passed in.
  # attribute: Used to select the regression parameter to use in the chart  May
  #   be any of:
  #     Alpha - shows the y-intercept
  #     Beta - shows the slope of the regression line
  #     R-Squared - shows the fit of the regression to the data
  #
  
  # Outputs:
  # A stack of three related timeseries line charts
  
  # FUNCTION:
  
  columns.a = ncol(Ra)
  columns.b = ncol(Rb)
  
  #     if(columns.a > 1 | columns.b > 1)
  #         legend.loc = "topleft"
  #     else
  #         legend.loc = NULL
  
  #    plot.new()
  
  op <- par(no.readonly=TRUE)
  
  layout(matrix(c(1,2,3)),heights=c(1.3,1,1.3),widths=1)
  
  par(mar=c(1,4,4,2))
  if(is.null(main)){
    freq = periodicity(Ra)
    
    switch(freq$scale,
           minute = {freq.lab = "minute"},
           hourly = {freq.lab = "hour"},
           daily = {freq.lab = "day"},
           weekly = {freq.lab = "week"},
           monthly = {freq.lab = "month"},
           quarterly = {freq.lab = "quarter"},
           yearly = {freq.lab = "year"}
    )
    
    main = paste("Rolling ",width,"-",freq.lab," Regressions", sep="")
  }
  
  rollingreg(Ra, Rb, width = width, Rf = Rf, attribute = "Alpha", xaxis = FALSE, main = main, ylab = "Alpha", legend.loc=legend.loc, event.labels = event.labels, ...)
  
  par(mar=c(1,4,0,2))
  
  rollingreg(Ra, Rb, width = width, Rf = Rf, attribute = "Beta", main = "", ylab = "Beta", xaxis = FALSE, event.labels = NULL, ...)
  
  par(mar=c(5,4,0,2))
  
  rollingreg(Ra, Rb, width = width, Rf = Rf, attribute = "R-Squared", main = "", ylab = "R-Squared", event.labels = NULL, ...)
  
  par(op)
}
