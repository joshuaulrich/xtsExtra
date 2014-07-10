
# Environment for our xts chart objects
.plotxtsEnv <- new.env()

current.chob <- function() invisible(get(".xts_chob",.plotxtsEnv))

# based on quantmod R/chart_Series.R

# chart_pars {{{
chart_pars <- function() {
  list(cex=0.6, mar=c(3,1,0,1))
} # }}}

chart.lines <- function(x, colorset=1:12){
  for(i in 1:NCOL(x))
    lines(1:NROW(x),x[,i],lwd=2,col=colorset[i],lend=3,lty=1)
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
                      panel="",
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
  
  # Do some checks on x
  if(is.character(x))
    stop("'x' must be a time-series object")
  
  # If we detect an OHLC object, we should call quantmod::chart_Series
  #if(is.OHLC(x)) {
  #  cs$Env$xdata <- OHLC(x)
  #  if(has.Vo(x))
  #    cs$Env$vo <- Vo(x)
  #} else 
  
  cs$Env$xdata <- x
  #subset <- match(.index(x[subset]), .index(x))
  cs$Env$xsubset <- subset
  cs$Env$cex <- pars$cex
  cs$Env$mar <- pars$mar
  cs$set_asp(3)
  
  # xlim and ylim are set based on cs$Env$xdata[subset]. How do we handle other
  # transformations (e.g. cumulative returns, correlations, etc.) as the
  # main panel
  cs$set_xlim(c(1,NROW(cs$Env$xdata[subset])))
  cs$set_ylim(list(structure(range(na.omit(cs$Env$xdata[subset])),fixed=FALSE)))
  
  cs$set_frame(1,FALSE)
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
  #cs$Env$type <- type
  
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
  #cs$set_frame(-1)
  # background of main window
  #cs$add(expression(rect(par("usr")[1],
  #                       par("usr")[3],
  #                       par("usr")[2],
  #                       par("usr")[4],border=NA,col=theme$bg)),expr=TRUE)
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
  cs$Env$name <- name
  text.exp <- c(expression(text(1-1/3,0.5,name,font=2,col='#444444',offset=0,cex=1.1,pos=4)),
                expression(text(NROW(xdata[xsubset]),0.5,
                                paste(start(xdata[xsubset]),end(xdata[xsubset]),sep=" / "),
                                col=1,adj=c(0,0),pos=2)))
  cs$add(text.exp, env=cs$Env, expr=TRUE)
  cs$set_frame(2)
  
  cs$Env$axis_labels <- function(xdata,xsubset,scale=5) {
    axTicksByValue(na.omit(xdata[xsubset]))
  }
  cs$Env$make_pretty_labels <- function(ylim) {
    p <- pretty(ylim,10)
    p[p > ylim[1] & p < ylim[2]]
  }
  #cs$add(assign("five",rnorm(10)))  # this gets re-evaled each update, though only to test
  #cs$add(expression(assign("alabels", axTicksByValue(na.omit(xdata[xsubset])))),expr=TRUE)
  #cs$add(expression(assign("alabels", pretty(range(xdata[xsubset],na.rm=TRUE)))),expr=TRUE)
  #cs$add(expression(assign("alabels", pretty(get_ylim(get_frame())[[2]],10))),expr=TRUE)
  cs$add(expression(assign("alabels", make_pretty_labels(get_ylim(get_frame())[[2]]))),expr=TRUE)
  
  # add $1 grid lines if appropriate
  #cs$set_frame(-2)
  
  # add minor y-grid lines
  #cs$add(expression(if(diff(range(xdata[xsubset],na.rm=TRUE)) < 50)
  #  segments(1,seq(min(xdata[xsubset]%/%1,na.rm=TRUE),
  #                 max(xdata[xsubset]%/%1,na.rm=TRUE),1),
  #           length(xsubset),
  #           seq(min(xdata[xsubset]%/%1,na.rm=TRUE),
  #               max(xdata[xsubset]%/%1,na.rm=TRUE),1),
  #           col=theme$grid2, lty="dotted")), expr=TRUE)
  
  cs$set_frame(2)
  # add main y-grid lines
  cs$add(expression(segments(1,alabels,NROW(xdata[xsubset]),alabels, col=theme$grid)),expr=TRUE)
  
  # left axis labels
  if(theme$lylab) {
    cs$add(expression(text(1-1/3-max(strwidth(alabels)),
                           alabels, #axis_labels(xdata,xsubset), 
                           noquote(format(alabels,justify="right")), 
                           col=theme$labels,offset=0,cex=0.9,pos=4,xpd=TRUE)),expr=TRUE)
  }
  
  # right axis labels
  if(theme$rylab) {
    cs$add(expression(text(NROW(xdata[xsubset])+1/3,
                           alabels, 
                           noquote(format(alabels,justify="right")),
                           col=theme$labels,offset=0,cex=0.9,pos=4,xpd=TRUE)),expr=TRUE)
  }
  
  # add main series
  cs$set_frame(2)
  cs$add(expression(chart.lines(xdata[xsubset])),expr=TRUE)
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
  
  assign(".xts_chob", cs, .plotxtsEnv)
  cs
} #}}}

addDrawdowns <- function(geometric=TRUE, ...){
  lenv <- new.env()
  lenv$plot_drawdowns <- function(x, geometric, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    drawdowns <- PerformanceAnalytics:::Drawdowns(xdata, geometric)[xsubset]
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
  xsubset <- plot_object$Env$xsubset
  drawdowns <- PerformanceAnalytics:::Drawdowns(plot_object$Env$xdata, geometric=geometric)
  lenv$xdata <- drawdowns
  lenv$xsubset <- subset
  
  # add the frame for drawdowns info
  plot_object$add_frame(ylim=c(0,1),asp=0.2)
  plot_object$next_frame()
  text.exp <- expression(text(c(1, 1+strwidth("Drawdowns")),
                              0.3,
                              c("Drawdowns", ""),
                              col=c(1,"gray"),adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(expression(rect(par("usr")[1],0,par("usr")[2],1,col=theme$grid,border="black")),expr=TRUE)
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
  
  # add frame for the actual drawdowns data
  plot_object$add_frame(ylim=range(drawdowns),asp=1,fixed=TRUE)
  plot_object$next_frame()
  
  # need to add gridlines and y-axis labels for this panel
  # using axis is easier, but does not have same formatting as other axes
  # exp <- c(exp, expression(axis(side = 2, at = pretty(range(xdata)))))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
}

