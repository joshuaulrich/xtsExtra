
# Environment for our xts chart objects
.plotxtsEnv <- new.env()

current.chob <- function() invisible(get(".xts_chob",.plotxtsEnv))

# based on quantmod R/chart_Series.R

# chart_pars {{{
chart_pars <- function() {
  list(cex=0.6, mar=c(3,2,0,2))
} # }}}

chart.lines <- function(x, colorset=1:12, type="l"){
  if(type == "h"){
    lines(1:NROW(x),x[,1],lwd=2,col=colorset[1],lend=3,lty=1, type="h")
  } else {
  for(i in 1:NCOL(x))
    lines(1:NROW(x),x[,i],lwd=2,col=colorset[i],lend=3,lty=1, type="l")
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
                        up.col=NA, 
                        dn.border="#333333", 
                        up.border="#333333"),
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
                      mainPanel=NULL,
                      panels=NULL,
                      byColumn=FALSE,
                      type="l",
                      name=deparse(substitute(x)), 
                      subset="", 
                      clev=0,
                      pars=chart_pars(), theme=xtsExtraTheme(),
                      ...){
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
  # cs$Env$R <- PerformanceAnalytics:::Drawdowns(x)
  # rough prototype for calling a function for the main "panel"
  if(!is.null(mainPanel)){
    FUN <- match.fun(mainPanel$name)
    args <- mainPanel$args
    .formals <- formals(FUN)
    .formals <- modify.args(formals=.formals, arglist=args, dots=TRUE)
    if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=R, dots=TRUE)
    .formals$... <- NULL
    R <- try(do.call(FUN, .formals), silent=TRUE)
    if(inherits(R, "try-error")) { 
      message(paste("mainPanel function failed with message", R))
      cs$Env$R <- x
    } else {
      cs$Env$R <- R
    }
  } else {
    cs$Env$R <- R
  }
  
  # xlim and ylim are set based on cs$Env$xdata[subset]. How do we handle other
  # transformations (e.g. cumulative returns, correlations, etc.) as the
  # main panel?
  # Set xlim based on the raw returns data passed into function
  cs$set_xlim(c(1,NROW(cs$Env$xdata[subset])))
  
  # Set ylim based on the transformed data
  cs$set_ylim(list(structure(range(na.omit(cs$Env$R[subset])),fixed=TRUE)))
  
  
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
  
  # add name and start/end dates
  cs$Env$name <- name
  text.exp <- c(expression(text(1-1/3,0.5,name,font=2,col='#444444',offset=0,cex=1.1,pos=4)),
                expression(text(NROW(xdata[xsubset]),0.5,
                                paste(start(xdata[xsubset]),end(xdata[xsubset]),sep=" / "),
                                col=1,adj=c(0,0),pos=2)))
  cs$add(text.exp, env=cs$Env, expr=TRUE)
  
  cs$set_frame(2)
  # define function for y-axis labels
  cs$Env$grid_lines <- function(xdata, xsubset) {
    ylim <- range(xdata[xsubset])
    p <- pretty(ylim, 10)
    p[p > ylim[1] & p < ylim[2]]
  }
  
  # add y-axis grid lines and labels
  exp <- c(
    # y-axis grid lines
    expression(segments(1, grid_lines(R,xsubset), NROW(xdata[xsubset]), grid_lines(R,xsubset),
                        col=theme$grid)),
    # left y-axis labels
    expression(text(1-1/3-max(strwidth(grid_lines(R,xsubset))), grid_lines(R,xsubset),
                    noquote(format(grid_lines(R,xsubset), justify="right")),
                    col=theme$labels, offset=0, pos=4, cex=0.9, xpd=TRUE)),
    # right y-axis labels
    expression(text(NROW(R[xsubset])+1/3, grid_lines(R,xsubset),
                    noquote(format(grid_lines(R,xsubset), justify="right")),
                    col=theme$labels, offset=0, pos=4, cex=0.9, xpd=TRUE))
  )
  cs$add(exp, env=cs$Env, expr=TRUE)
  
  # add main series
  cs$set_frame(2)
  if(isTRUE(byColumn)){
    # Add expression for the main plot
    cs$add(expression(chart.lines(R[,1][xsubset], type=type)),expr=TRUE)
    for(i in 2:NCOL(x)){
      # create a local environment
      lenv <- new.env()
      lenv$xdata <- cs$Env$R[,i][subset]
      lenv$name <- cs$Env$colum_names[i]
      lenv$ylim <- range(cs$Env$R[subset])
      lenv$type <- cs$Env$type
      
      # Add a small frame for the time series info
      cs$add_frame(ylim=c(0,1),asp=0.2)
      cs$next_frame()
      text.exp <- expression(text(x=c(1,1+strwidth(name)),
                                  y=0.3,
                                  labels=c(name,""),
                                  col=c(1,1),adj=c(0,0),cex=0.9,offset=0,pos=4))
      cs$add(text.exp, env=c(lenv,cs$Env), expr=TRUE)
      
      # Add the frame for the sub-plots
      # Set the ylim based on the (potentially) transformed data in cs$Env$R
      cs$add_frame(ylim=range(cs$Env$R[cs$Env$xsubset]), asp=NCOL(cs$Env$xdata), fixed=TRUE)
      cs$next_frame()
      
      exp <- expression(chart.lines(xdata[xsubset], type=type))
      
      # define function to plot the y-axis grid lines
      lenv$y_grid_lines <- function(ylim) { 
        #pretty(range(xdata[xsubset]))
        p <- pretty(ylim,10)
        p[p > ylim[1] & p < ylim[2]]
      }
      
      exp <- c(expression(
        # y-axis grid lines
        segments(1,y_grid_lines(ylim),NROW(xdata[xsubset]), y_grid_lines(ylim),
                 col=theme$grid)), # add y-axis grid lines
        exp,  # NOTE 'exp' was defined earlier
        # y-axis labels/boxes
        expression(text(1-1/3-max(strwidth(y_grid_lines(ylim))), y_grid_lines(ylim),
                        noquote(format(y_grid_lines(ylim),justify="right")),
                        col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)),
        expression(text(NROW(xdata[xsubset])+1/3, y_grid_lines(ylim),
                        noquote(format(y_grid_lines(ylim),justify="right")),
                        col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)),
        # x-axis grid lines
        expression(atbt <- axTicksByTime2(xdata[xsubset]),
                   segments(atbt, #axTicksByTime2(xdata[xsubset]),
                            ylim[1],
                            atbt, #axTicksByTime2(xdata[xsubset]),
                            ylim[2], col=theme$grid)))
      cs$add(exp,env=c(lenv, cs$Env),expr=TRUE,no.update=TRUE)
    }
  } else {
    cs$add(expression(chart.lines(R[xsubset])),expr=TRUE)
  }
  assign(".xts_chob", cs, .plotxtsEnv)
  
  # Plot the panels or default to a simple line chart
  #if(!is.null(panel) && nchar(panel) > 0) {
  #  panel <- parse(text=panel, srcfile=NULL)
  #  for( p in 1:length(panel)) {
  #    if(length(panel[p][[1]][-1]) > 0) {
  #      cs <- eval(panel[p])
  #    } else {
  #      cs <- eval(panel[p])
  #    }
  #  }
  #} else {
  #  cs$add(expression(chart.lines(xdata[xsubset])),expr=TRUE)
  #}
  # assign(".xts_chob", cs, .plotxtsEnv)
  
  cs
} #}}}

addDrawdowns <- function(geometric=TRUE, col=1, ...){
  lenv <- new.env()
  lenv$name <- "Drawdowns"
  lenv$plot_drawdowns <- function(x, geometric, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    # Add x-axis grid lines
    segments(axTicksByTime2(xdata[xsubset]),
             par("usr")[3],
             axTicksByTime2(xdata[xsubset]),
             par("usr")[4],
             col=x$Env$theme$grid)
    drawdowns <- PerformanceAnalytics:::Drawdowns(xdata, geometric)
    chart.lines(drawdowns) 
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(geometric=geometric,...)),
         list(geometric=geometric,...))
  exp <- parse(text=gsub("list","plot_drawdowns",
                         as.expression(substitute(list(x=current.chob(),
                                                       geometric=geometric,...)))),
               srcfile=NULL)
  
  plot_object <- current.chob()
  xdata <- plot_object$Env$xdata
  #xsubset <- plot_object$Env$xsubset
  
  drawdowns <- PerformanceAnalytics:::Drawdowns(plot_object$Env$xdata, geometric=geometric)
  lenv$xdata <- drawdowns
  lenv$col <- col
  
  # add the frame for drawdowns info
  plot_object$add_frame(ylim=c(0,1),asp=0.25)
  plot_object$next_frame()
  text.exp <- expression(text(x=c(1,1+strwidth(name)),
                              y=0.3,
                              labels=c(name,""),
                              col=c(1,col),adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
  
  # add frame for the actual drawdowns data
  plot_object$add_frame(ylim=range(na.omit(drawdowns)),asp=1,fixed=TRUE)
  plot_object$next_frame()
  
  lenv$grid_lines <- function(xdata,xsubset) {
    ylim <- range(xdata[xsubset])
    p <- pretty(ylim, 10)
    p[p > ylim[1] & p < ylim[2]]
  }
  # add y-axis gridlines and labels
  exp <- c(expression(segments(1,grid_lines(xdata,xsubset),NROW(xdata[xsubset]),grid_lines(xdata,xsubset),
                               col=theme$grid)), 
           exp,  # NOTE 'exp' was defined earlier
           # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
                           noquote(format(grid_lines(xdata,xsubset),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)),
           expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
                           noquote(format(grid_lines(xdata,xsubset),justify="right")),
                           col=theme$labels,offset=0,pos=4,cex=0.9, xpd=TRUE)))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  plot_object
}

# based on quantmod::add_TA
add_Lines <- function(x, name="", order=NULL, on=NA, legend="auto",
                      yaxis=list(NULL,NULL),
                      col=1, type="l", ...) { 
  lenv <- new.env()
  lenv$name <- name
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
                         as.expression(substitute(list(x=current.chob(),
                                                       ta=get("x"),on=on,
                                                       type=type,col=col,...)))),
               srcfile=NULL)
  plot_object <- current.chob()
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
                                labels=name,
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


