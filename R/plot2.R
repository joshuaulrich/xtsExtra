
# Environment for our xts chart objects
.plotxtsEnv <- new.env()

current.xts_chob <- function() invisible(get(".xts_chob",.plotxtsEnv))

# based on quantmod R/chart_Series.R

# chart_pars {{{
chart_pars <- function() {
  list(cex=0.6, mar=c(3,2,0,2))
} # }}}

chart.lines <- function(x, type="l", colorset=1:10, up.col=NULL, dn.col=NULL){
  if(is.null(up.col)) up.col <- "green"
  if(is.null(dn.col)) dn.col <- "red"
  if(type == "h"){
    colors <- ifelse(x[,1] < 0, dn.col, up.col)
    lines(1:NROW(x),x[,1],lwd=2,col=colors,lend=1,lty=1,type="h")
  } else {
    for(i in 1:NCOL(x)){
      lines(1:NROW(x),x[,i],lwd=2,col=colorset[i],lend=1,lty=1,type="l")
    }
  }
}

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

plot2_xts <- function(x, 
                      FUN=NULL,
                      panels=NULL,
                      byColumn=FALSE,
                      type="l",
                      main=deparse(substitute(x)), 
                      subset="", 
                      clev=0,
                      pars=chart_pars(), theme=xtsExtraTheme(),
                      ylim=NULL,
                      ...){
  
  # Small multiples with multiple pages behavior occurs when byColumn is
  # an integer. (i.e. bycolumn=2 means to iterate over the data in a step
  # size of 2 and plot 2 panels on each page
  # Make recursive calls and return
  if(is.numeric(byColumn)){
    byColumn <- min(NCOL(x), byColumn)
    idx <- seq.int(1L, NCOL(x), 1L)
    chunks <- split(idx, ceiling(seq_along(idx)/byColumn))
    
    if(!is.null(panels) && nchar(panels) > 0){
      # we will plot the panels, but not plot the returns by column
      byColumn <- FALSE
    } else {
      # we will plot the returns by column, but not the panels
      byColumn <- TRUE
      panels <- NULL
      FUN <- NULL
      ylim <- range(na.omit(x[subset]))
    }
    
    for(i in 1:length(chunks)){
      tmp <- chunks[[i]]
      p <- plot2_xts(x=x[,tmp], FUN=FUN, panels=panels, 
                     byColumn=byColumn, type=type, main=main, subset=subset, 
                     clev=clev, pars=pars, theme=theme, ylim=ylim, ...=...)
      if(i < length(chunks))
        print(p)
    }
    # NOTE: return here so we don't draw another chart
    return(p)
  }
  
  cs <- new.replot_xts()
  #cex <- pars$cex
  #mar <- pars$mar
  line.col <- theme$col$line.col
  up.col <- theme$col$up.col
  dn.col <- theme$col$dn.col
  up.border <- theme$col$up.border
  dn.border <- theme$col$dn.border
  format.labels <- theme$format.labels
  if(is.null(theme$grid.ticks.on)) {
    xs <- x[subset]
    major.grid <- c(years=nyears(xs),
                    months=nmonths(xs),
                    days=ndays(xs))
    grid.ticks.on <- names(major.grid)[rev(which(major.grid < 30))[1]]
  } else grid.ticks.on <- theme$grid.ticks.on
  label.bg <- theme$col$label.bg
  
  # define a subset function
  cs$subset <- function(x) {
    if(FALSE) {set_ylim <- get_ylim <- set_xlim <- Env <-function(){} }  # appease R parser?
    if(missing(x)) {
      x <- "" #1:NROW(Env$xdata)
    }
    Env$xsubset <<- x
    set_xlim(c(1,NROW(Env$xdata[Env$xsubset])))
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
               min.tmp <- min(ylim[[frame]][1],range(na.omit(lenv$xdata[Env$xsubset]))[1],na.rm=TRUE)
               max.tmp <- max(ylim[[frame]][2],range(na.omit(lenv$xdata[Env$xsubset]))[2],na.rm=TRUE)
               ylim[[frame]] <<- structure(c(min.tmp,max.tmp),fixed=fixed)
             }
           })
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv, lenv
    set_ylim(ylim)
  }
  environment(cs$subset) <- environment(cs$get_asp)
  
  # add theme and charting parameters to Env
  if(byColumn){
    cs$set_asp(NCOL(x))
  } else {
    cs$set_asp(3)
  }
  cs$Env$cex <- pars$cex
  cs$Env$mar <- pars$mar
  cs$Env$clev = min(clev+0.01,1) # (0,1]
  cs$Env$theme$bbands <- theme$bbands
  cs$Env$theme$shading <- theme$shading
  cs$Env$theme$line.col <- theme$col$line.col
  cs$Env$theme$up.col <- up.col
  cs$Env$theme$dn.col <- dn.col
  cs$Env$theme$up.border <- up.border
  cs$Env$theme$dn.border <- dn.border
  cs$Env$theme$colorset <- theme$col$colorset
  cs$Env$theme$rylab <- theme$rylab
  cs$Env$theme$lylab <- theme$lylab
  cs$Env$theme$bg <- theme$col$bg
  cs$Env$theme$grid <- theme$col$grid
  cs$Env$theme$grid2 <- theme$col$grid2
  cs$Env$theme$labels <- "#333333"
  cs$Env$theme$label.bg <- label.bg
  cs$Env$format.labels <- format.labels
  cs$Env$ticks.on <- grid.ticks.on
  cs$Env$grid.ticks.lwd <- theme$grid.ticks.lwd
  cs$Env$type <- type
  
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
  
  # Compute transformation if specified by panel argument
  # rough prototype for calling a function for the main "panel"
  if(!is.null(FUN)){
    fun <- match.fun(FUN)
    .formals <- formals(fun)
    .formals <- modify.args(formals=.formals, arglist=list(...), dots=TRUE)
    if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=x, dots=TRUE)
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
  cs$set_xlim(c(1,NROW(cs$Env$xdata[subset])))
  
  # Set ylim based on the transformed data
  # chart_Series uses fixed=FALSE and add_* uses fixed=TRUE, not sure why or
  # which is best.
  if(is.null(ylim)){
    cs$set_ylim(list(structure(range(na.omit(cs$Env$R[subset])),fixed=TRUE)))
    cs$Env$constant_ylim <- range(na.omit(cs$Env$R[subset]))
  } else {
    cs$set_ylim(list(structure(ylim, fixed=TRUE)))
    cs$Env$constant_ylim <- ylim
  }
  
  cs$set_frame(1,FALSE)
  # axis_ticks function to label lower frequency ranges/grid lines
  cs$Env$axis_ticks <- function(xdata,xsubset) {
    ticks <- diff(axTicksByTime2(xdata[xsubset],labels=FALSE))/2 + 
      last(axTicksByTime2(xdata[xsubset],labels=TRUE),-1)
    if(!theme$coarse.time || length(ticks) == 1)
      return(unname(ticks))
    if(min(diff(ticks)) < max(strwidth(names(ticks)))) {
      ticks <- unname(ticks)
    }
    ticks
  }
  
  # compute the x-axis ticks
  # need to add if(upper.x.label) to allow for finer control
  cs$add(expression(atbt <- axTicksByTime2(xdata[xsubset]),
                    segments(atbt, #axTicksByTime2(xdata[xsubset]),
                             get_ylim()[[2]][1],
                             atbt, #axTicksByTime2(xdata[xsubset]),
                             get_ylim()[[2]][2], col=theme$grid, lwd=grid.ticks.lwd),
                    axt <- axis_ticks(xdata,xsubset),
                    text(as.numeric(axt),
                         par('usr')[3]-0.2*min(strheight(axt)),
                         names(axt),xpd=TRUE,cex=0.9,pos=3)),
         clip=FALSE,expr=TRUE)
  
  # Add frame for the chart "header" to display the name and start/end dates
  cs$add_frame(0,ylim=c(0,1),asp=0.2)
  cs$set_frame(1)
  
  # add observation level ticks on x-axis if < 400 obs.
  cs$add(expression(if(NROW(xdata[xsubset])<400) 
  {axis(1,at=1:NROW(xdata[xsubset]),labels=FALSE,col=theme$grid2,tcl=0.3)}),expr=TRUE)
  
  # add "month" or "month.abb"
  cs$add(expression(axt <- axTicksByTime(xdata[xsubset],format.labels=format.labels),
                    axis(1,at=axt, #axTicksByTime(xdata[xsubset]),
                         labels=names(axt), #axTicksByTime(xdata[xsubset],format.labels=format.labels)),
                         las=1,lwd.ticks=1,mgp=c(3,1.5,0),tcl=-0.4,cex.axis=.9)),
         expr=TRUE)
  
  # add main and start/end dates
  if((isTRUE(byColumn)) | (byColumn == 1) | (NCOL(x) == 1))
    cs$Env$main <- cs$Env$column_names[1] else cs$Env$main <- main
  
  text.exp <- c(expression(text(1-1/3,0.5,main,font=2,col='#444444',offset=0,cex=1.1,pos=4)),
                expression(text(NROW(xdata[xsubset]),0.5,
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
  exp <- expression(segments(1, y_grid_lines(constant_ylim), NROW(xdata[xsubset]), 
                             y_grid_lines(constant_ylim), col=theme$grid))
  if(theme$lylab){
    exp <- c(exp, 
             # left y-axis labels
             expression(text(1-1/3-max(strwidth(y_grid_lines(constant_ylim))), 
                             y_grid_lines(constant_ylim),
                             noquote(format(y_grid_lines(constant_ylim), justify="right")),
                             col=theme$labels, offset=0, pos=4, cex=0.9, xpd=TRUE)))
  }
  if(theme$rylab){
    exp <- c(exp, 
             # right y-axis labels
             expression(text(NROW(R[xsubset])+1/3, y_grid_lines(constant_ylim),
                             noquote(format(y_grid_lines(constant_ylim), justify="right")),
                             col=theme$labels, offset=0, pos=4, cex=0.9, xpd=TRUE)))
  }
  cs$add(exp, env=cs$Env, expr=TRUE)
  
  # add main series
  cs$set_frame(2)
  if(isTRUE(byColumn)){
    # We need to plot the first "panel" here because the plot area is
    # set up based on the code above
    lenv <- new.env()
    lenv$xdata <- cs$Env$R[,1][subset]
    lenv$main <- cs$Env$colum_names[1]
    #lenv$ymax <- range(cs$Env$R[subset])[2]
    lenv$type <- cs$Env$type
    exp <- expression(chart.lines(xdata, type=type, colorset=theme$colorset, 
                                  up.col=theme$up.col, dn.col=theme$dn.col))
    #exp <- c(exp, expression(text(1, ymax, adj=c(0,0), pos=4, cex=0.9, offset=0, labels=main)))
    # Add expression for the main plot
    cs$add(exp, env=c(lenv,cs$Env), expr=TRUE)
    
    if(NCOL(cs$Env$xdata) > 1){
      for(i in 2:NCOL(cs$Env$xdata)){
        # create a local environment
        lenv <- new.env()
        lenv$xdata <- cs$Env$R[,i][subset]
        lenv$main <- cs$Env$column_names[i]
        lenv$ylim <- cs$Env$constant_ylim
        lenv$type <- cs$Env$type
        
        # Add a small frame for the time series info
        cs$add_frame(ylim=c(0,1),asp=0.2)
        cs$next_frame()
        text.exp <- expression(text(x=1,
                                    y=0.5,
                                    labels=main,
                                    adj=c(0,0),cex=0.9,offset=0,pos=4))
        cs$add(text.exp, env=c(lenv,cs$Env), expr=TRUE)
        
        # Add the frame for the sub-plots
        # Set the ylim based on the (potentially) transformed data in cs$Env$R
        cs$add_frame(ylim=cs$Env$constant_ylim, asp=NCOL(cs$Env$xdata), fixed=TRUE)
        cs$next_frame()
        
        exp <- expression(chart.lines(xdata[xsubset], type=type, 
                                      colorset=theme$colorset, 
                                      up.col=theme$up.col, 
                                      dn.col=theme$dn.col))
        
        # define function to plot the y-axis grid lines
        lenv$y_grid_lines <- function(ylim) { 
          #pretty(range(xdata[xsubset]))
          p <- pretty(ylim,5)
          p[p > ylim[1] & p < ylim[2]]
        }
        
        # NOTE 'exp' was defined earlier as chart.lines
        exp <- c(exp, 
                 # y-axis grid lines
                 expression(segments(1,y_grid_lines(ylim),NROW(xdata[xsubset]), 
                                     y_grid_lines(ylim), col=theme$grid)),
                 # x-axis grid lines
                 expression(atbt <- axTicksByTime2(xdata[xsubset]),
                            segments(atbt, #axTicksByTime2(xdata[xsubset]),
                                     ylim[1],
                                     atbt, #axTicksByTime2(xdata[xsubset]),
                                     ylim[2], col=theme$grid)))
        if(theme$lylab){
          exp <- c(exp, 
                   # y-axis labels/boxes
                   expression(text(1-1/3-max(strwidth(y_grid_lines(ylim))), y_grid_lines(ylim),
                                   noquote(format(y_grid_lines(ylim),justify="right")),
                                   col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
        }
        if(theme$rylab){
          exp <- c(exp, 
                   expression(text(NROW(xdata[xsubset])+1/3, y_grid_lines(ylim),
                                   noquote(format(y_grid_lines(ylim),justify="right")),
                                   col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
        }
        cs$add(exp,env=c(lenv, cs$Env),expr=TRUE,no.update=TRUE)
      }
  }
  } else {
    cs$add(expression(chart.lines(R[xsubset], type=type, 
                                  colorset=theme$colorset,
                                  up.col=theme$up.col, 
                                  dn.col=theme$dn.col)),expr=TRUE)
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
    segments(axTicksByTime2(xdata[xsubset]),
             par("usr")[3],
             axTicksByTime2(xdata[xsubset]),
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
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  
  drawdowns <- PerformanceAnalytics:::Drawdowns(plot_object$Env$xdata, geometric=geometric)
  lenv$xdata <- drawdowns
  
  # add the frame for drawdowns info
  plot_object$add_frame(ylim=c(0,1),asp=0.25)
  plot_object$next_frame()
  text.exp <- expression(text(x=1, y=0.3, labels=main,
                              col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
  
  # add frame for the actual drawdowns data
  if(is.null(ylim)) {
    ylim <- range(na.omit(lenv$xdata[xsubset]))
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
  exp <- c(expression(segments(1,grid_lines(ylim),NROW(xdata[xsubset]),grid_lines(ylim),
                               col=theme$grid)), 
           exp,  # NOTE 'exp' was defined earlier
           # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(ylim))),grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)),
           expression(text(NROW(xdata[xsubset])+1/3,grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  plot_object
}

# based on quantmod::add_TA
addLines <- function(x, main="", order=NULL, on=NA, legend="auto",
                     yaxis=list(NULL,NULL),
                     col=1, type="l", ...) { 
  lenv <- new.env()
  lenv$main <- main
  lenv$plot_ta <- function(x, ta, on, type, col,...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    if(all(is.na(on))) {
      # x-axis grid lines based on Env$xdata and Env$xsubset
      segments(axTicksByTime2(xdata[xsubset]),
               par("usr")[3],
               axTicksByTime2(xdata[xsubset]),
               par("usr")[4],
               col=x$Env$theme$grid)
    }
    if(is.logical(ta)) {
      ta <- merge(ta, xdata, join="right",retside=c(TRUE,FALSE))[xsubset]
      shade <- shading(as.logical(ta,drop=FALSE))
      if(length(shade$start) > 0) # all FALSE cause zero-length results
        rect(shade$start-1/3, par("usr")[3] ,shade$end+1/3, par("usr")[4], col=col,...) 
    } else {
      # we can add points that are not necessarily at the points
      # on the main series
      subset.range <- paste(start(x$Env$xdata[x$Env$xsubset]),
                            end(x$Env$xdata[x$Env$xsubset]),sep="/")
      ta.adj <- merge(n=.xts(1:NROW(x$Env$xdata[x$Env$xsubset]),
                             .index(x$Env$xdata[x$Env$xsubset]), tzone=indexTZ(x$Env$xdata)),ta)[subset.range]
      ta.x <- as.numeric(na.approx(ta.adj[,1], rule=2) )
      ta.y <- ta.adj[,-1]
      chart.lines(ta.y, colorset=col, type=type)
    }
  }
  lenv$xdata <- x
  # map all passed args (if any) to 'lenv' environment
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(x=x,order=order,on=on,legend=legend,
                    type=type,col=col,...)),
         list(x=x,order=order,on=on,legend=legend,
              type=type,col=col,...))
  exp <- parse(text=gsub("list","plot_ta",
                         as.expression(substitute(list(x=current.xts_chob(),
                                                       ta=get("x"),on=on,
                                                       type=type,col=col,...)))),
               srcfile=NULL)
  plot_object <- current.xts_chob()
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  if(is.logical(x)) no.update <- TRUE else no.update <- FALSE
  #  this merge isn't going to work if x isn't in xdata range. Something like:
  #    na.approx(merge(n=.xts(1:NROW(xdata),.index(xdata)),ta)[,1])
  #  should allow for any time not in the original to be merged in.
  #  probably need to subset xdata _before_ merging, else subset will be wrong
  #
  #tav <- merge(x, xdata, join="right",retside=c(TRUE,FALSE))
  #lenv$xdata <- tav
  #tav <- tav[xsubset]
  lenv$col <- col
  lenv$xdata <- merge(x,xdata,retside=c(TRUE,FALSE))
  
  if(is.na(on)) {
    plot_object$add_frame(ylim=c(0,1),asp=0.2)
    plot_object$next_frame()
    text.exp <- expression(text(x=1,
                                y=0.3,
                                labels=main,
                                col=c(1,col),adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
    
    plot_object$add_frame(ylim=range(na.omit(xdata)),asp=1)  # need to have a value set for ylim
    plot_object$next_frame()
    # add grid lines, using custom function for MACD gridlines
    lenv$grid_lines <- function(xdata,xsubset) { 
      pretty(range(xdata[xsubset]))
    }
    exp <- c(expression(segments(1,grid_lines(xdata,xsubset),NROW(xdata[xsubset]),grid_lines(xdata,xsubset),
                                 col=theme$grid)), exp,  # NOTE 'exp' was defined earlier to be plot_macd
             # add axis labels/boxes
             expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
                             noquote(format(grid_lines(xdata,xsubset),justify="right")),
                             col=theme$labels,offset=0,pos=4,cex=0.9,xpd=TRUE)),
             expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
                             noquote(format(grid_lines(xdata,xsubset),justify="right")),
                             col=theme$labels,offset=0,pos=4,cex=0.9,xpd=TRUE)))
    plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
  } else { 
    for(i in 1:length(on)) {
      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      lenv$grid_lines <- function(xdata,xsubset) { 
        pretty(range(xdata[xsubset]))
      }
      exp <- c(exp,
               # LHS
               #expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
               #           noquote(format(grid_lines(xdata,xsubset),justify="right")),
               #           col=theme$labels,offset=0,pos=4,cex=0.9)),
               # RHS
               expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
                               noquote(format(grid_lines(xdata,xsubset),justify="right")),
                               col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
      #}
      plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
    }
  }
  plot_object
} #}}}

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
    segments(axTicksByTime2(xdata[xsubset]),
             par("usr")[3],
             axTicksByTime2(xdata[xsubset]),
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
  
  # get the raw returns data
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  
  # add data to the local environment
  lenv$xdata <- xdata
  lenv$xsubset <- xsubset
  lenv$col <- col
  lenv$type <- type
  
  # add the frame for time series info
  plot_object$add_frame(ylim=c(0,1),asp=0.25)
  plot_object$next_frame()
  text.exp <- expression(text(x=1, y=0.3, labels=main,
                              col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
  
  # add frame for the actual data
  if(is.null(ylim)) {
    ylim <- range(na.omit(lenv$xdata[xsubset]))
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
  exp <- c(expression(segments(1,grid_lines(ylim),NROW(xdata[xsubset]),
                               grid_lines(ylim),col=theme$grid)), 
           exp,  # NOTE 'exp' was defined earlier
           # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(ylim))),grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)),
           expression(text(NROW(xdata[xsubset])+1/3,grid_lines(ylim),
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
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  
  rolling_performance <- RollingPerformance(R=plot_object$Env$xdata, width=width, FUN=FUN, ...=..., fill=fill)
  lenv$xdata <- rolling_performance
  lenv$col <- col
  
  # add the frame for drawdowns info
  plot_object$add_frame(ylim=c(0,1),asp=0.25)
  plot_object$next_frame()
  text.exp <- expression(text(x=1, y=0.3, labels=main,
                              adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
  
  # add frame for the actual drawdowns data
  if(is.null(ylim)) {
    ylim <- range(na.omit(lenv$xdata[xsubset]))
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
  exp <- c(expression(segments(1,grid_lines(ylim),NROW(xdata[xsubset]),
                               grid_lines(ylim),col=theme$grid)), 
           exp,  # NOTE 'exp' was defined earlier
           # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(ylim))),grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)),
           expression(text(NROW(xdata[xsubset])+1/3,grid_lines(ylim),
                           noquote(format(grid_lines(ylim),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  plot_object
}
