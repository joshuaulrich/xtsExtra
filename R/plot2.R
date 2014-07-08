

# Environment for our xts chart objects
.plotxtsEnv <- new.env()

new.chob <- function(frame=1, xlim=c(1,10), ylim=list(structure(c(1,10), fixed=FALSE))){
  # This function is modeled after quantmod::new.replot
  Env <- new.env()
  
  # Not exactly sure what frame is doing or if I need it
  Env$frame <- frame
  # Env$asp <- asp
  
  # xlim should always remain constant and be used for each subsequent plot
  Env$xlim <- xlim
  
  # ylim is a list where
  # ylim[[1]] --> data[[1]], ..., ylim[[n]] --> data[[n]]
  Env$ylim <- ylim
  
  
  Env$pad1 <- 0.25 # bottom padding per frame
  Env$pad3 <- 0.25 # top padding per frame 
  
  ##### setters #####
  # set_frame <- function(frame,clip=TRUE) {
  #   Env$frame <<- frame
  #   #set_window(clip) # change actual window
  # }
  # set_frame <- function(frame) { Env$frame <<- frame }
  # set_asp <- function(asp) { Env$asp <<- asp }
  set_xlim <- function(xlim) { Env$xlim <<- xlim }
  set_ylim <- function(ylim) { Env$ylim <<- ylim }
  set_pad <- function(pad) { Env$pad1 <<- pad[1]; Env$pad3 <<- pad[2] }
  
  ##### getters #####
  # get_frame <- function(frame) { Env$frame }
  # get_asp   <- function(asp) { Env$asp }
  get_xlim  <- function(xlim) { Env$xlim }
  get_ylim  <- function(ylim) { Env$ylim }
  get_pad   <- function() c(Env$pad1,Env$pad3)
  
  # panels is a list where each element (i.e. slot) is what we want to evaluate
  Env$panels <- list()
  
  # add an expression to Env$panels (i.e. similar to Env$actions in quantmod)
  add <- function(x, env=Env, expr=FALSE, panel=NULL, ...) {
    if(!expr) {
      x <- match.call()$x
    }
    # each element in the Env$panels list is an object with "frame" and "env"
    # as environments
    a <- structure(x, env=env, ...)
    if(is.null(panel)){
      Env$panels[[length(Env$panels)+1]] <<- a
    } else {
      Env$panels[[panel]] <<- a
    }
  }
  
  # create a new environment that contains Env as one of its elements
  plotxts_env <- new.env()
  class(plotxts_env) <- c("plotxts", "environment")  
  plotxts_env$Env <- Env
  
  # add the setters to the plotxts_env environment
  # plotxts_env$set_frame <- set_frame
  # plotxts_env$set_asp <- set_asp
  plotxts_env$set_xlim <- set_xlim
  plotxts_env$set_ylim <- set_ylim
  plotxts_env$set_pad <- set_pad
  
  # add the getters to the plotxts_env environment
  # plotxts_env$get_frame <- get_frame
  # plotxts_env$get_asp <- get_asp
  plotxts_env$get_xlim <- get_xlim
  plotxts_env$get_ylim <- get_ylim
  plotxts_env$get_pad <- get_pad
  
  plotxts_env$add <- add
  #plotxts_env$add_frame <- add_frame
  #plotxts_env$update_frames <- update_frames
  #plotxts_env$add_frame <- add_frame
  #plotxts_env$next_frame <- next_frame
  return(plotxts_env)
}

# get the current chart object
current.chob <- function(){ invisible(get(".xts_chob", .plotxtsEnv)) }

# obviously need a better function name here
#' @param xts object of returns
#' @param byColumn 
#' @param layout a layout specification created with \code{\link{chartLayout}}
plot2_xts <- function(R, panels=NULL, byColumn=FALSE, layout=NULL, ...){
  # this function is modeled after quantmod::chart_Series
  # initialize a new chart object
  cs <- new.chob()
  
  # Env$R will hold the original returns object passed in
  cs$Env$R <- R
  cs$Env$byColumn <- byColumn
  cs$Env$layout <- layout
  
  
  cs$set_xlim(c(1, NROW(cs$Env$R)))
  cs$set_ylim(list(structure(range(na.omit(cs$Env$R)),fixed=FALSE)))
  
  # We should also do stuff here to get a common x-axis to use for each panel
  # or chart to work with specifying multiples
  # cs$set_xaxis()
  
  # Default plot behavior
  # create a local environment to add the ... 
  
  # the main plot will be added as an expression to Env$panels
  if(isTRUE(byColumn)){
    cnames <- colnames(R)
    for(i in 1:NCOL(R)){
      # create a local environment to add the args for chart.TimeSeries and
      # add as an expression 
      lenv <- new.env()
      lenv$args <- formals(chart.TimeSeries)
      lenv$args <- modify.args(lenv$args, R=R[,i], dots=TRUE)
      lenv$args <- modify.args(lenv$args, arglist=list(...), dots=TRUE)
      lenv$args$xaxis <- FALSE
      lenv$args$ylim <- cs$Env$ylim[[1]]
      lenv$args$main <- ""
      lenv$args$ylab <- cnames[i]
      # Plot the y axis on the right for even panels
      if(i %% 2 == 0){
        lenv$args$yaxis.right <- TRUE
      } else {
        lenv$args$yaxis.right <- FALSE
      }
      lenv$args$`...` <- NULL
      cs$add(expression(do.call(chart.TimeSeries, args)), env=c(lenv, cs$Env), expr=TRUE)
    }
  } else {
    # create a local environment to add the args for chart.TimeSeries
    lenv <- new.env()
    lenv$args <- formals(chart.TimeSeries)
    lenv$args <- modify.args(lenv$args, R=R, dots=TRUE)
    lenv$args <- modify.args(lenv$args, arglist=list(...), dots=TRUE)
    lenv$args$xaxis <- FALSE
    lenv$args$`...` <- NULL
    cs$add(expression(do.call(chart.TimeSeries, args)), env=c(lenv, cs$Env), expr=TRUE)
  }
  
  assign(".xts_chob", cs, .plotxtsEnv)
  cs
}

# print/plot
print.plotxts <- function(x, ...) plot.plotxts(x,...)
plot.plotxts <- function(x, ...){
  
  # Restore old par() options from what I change in here
  old.par <- par(c("mar", "oma"))
  on.exit(par(old.par))
  
  # plot.new()
  
  # Here we assign x to the .plotxtsEnv
  # x should have all of the data we need for plotting, layouts, etc
  assign(".xts_chob", x, .plotxtsEnv)
  
  # .formals <- x$Env$.formals
  # R <- x$Env$R
  pad1 <- x$Env$pad1
  pad3 <- x$Env$pad3
  
  par.list <- list(list(mar=c(pad1, 4, pad3, 3), oma=c(3.5, 0, 4, 0)),
                   list(mar=c(pad1, 4, pad3, 3)),
                   list(mar=c(pad1, 4, pad3, 3)))
  
  # Set the layout based on the number of panels or layout object
  npanels <- length(x$Env$panels)
  equal.heights <- ifelse(isTRUE(x$Env$byColumn), TRUE, FALSE)
  if(is.null(x$Env$layout)){
    cl <- updateLayout(npanels, equal.heights)
  } else {
    # The user has passed in something for layout
    if(!inherits(x$Env$layout, "chart.layout")){
      cl <- updateLayout(npanels, equal.heights)
    } else {
      cl <- x$Env$layout
    }
  }
  do.call(layout, cl)
  
  if(npanels > 1) {
    do.call(par, par.list[[1]]) 
  } else {
    # Use the default 
    par(mar=c(5,4,4,2)+0.1)
  }
  
  # Loop through the list in panels and evaluate each expression in its 
  # respective environment
  for(i in 1:npanels){
    if(npanels > 1){
      if(i == npanels){
        do.call('par', par.list[[3]])
      } else {
        do.call('par', par.list[[2]])
      }
    }
    aob <- x$Env$panels[[i]]
    env <- attr(aob, "env")
    if(is.list(env)) {
      # if env is c(lenv, Env), convert to list
      env <- unlist(lapply(env, function(x) eapply(x, eval)), recursive=FALSE)
    }
    eval(aob, env)
  }
  
  # add the x-axis at the very end here
  # We should functionalize this and provide for different options to plot
  # the x-axis as in quantmod or as in chart.TimeSeries
  ep <- xtsExtra:::axTicksByTime(x$Env$R)
  cex.axis <- 0.8
  label.height <- cex.axis * (0.5 + apply(t(names(ep)), 1, function(X) max(strheight(X, units="in") / par('cin')[2])))
  xaxis.labels <- names(ep)
  axis(1, at=ep, labels=xaxis.labels, las=1, lwd=1, mgp=c(3, label.height, 0))
  
  # reset the layout
  layout(matrix(1))
}

# layout functions modeled after quantmod
chartLayout <- function(mat, widths, heights){
  structure(list(mat=mat,
                 widths=widths,
                 heights=heights),
            class="chart.layout")
}

updateLayout <- function(x, equal.heights=FALSE){
  # x : number of panels
  if(x==1) {
    mat <- matrix(1)
    wd  <- 1
    ht  <- 1
  } else {
    mat <- matrix(1:x, x, 1, byrow=TRUE)
    wd  <- 1
    if(equal.heights){
      ht <- 1
    } else {
      # ht  <- c(3,rep(1,x-2),1.60)
      ht  <- c(3,rep(1,x-2),1)
    }
  }
  chartLayout(mat, wd, ht)
}

addDrawdowns <- function(geometric=TRUE, ...){
  lenv <- new.env()
  lenv$plot_drawdowns <- function(x, geometric, ...) {
    xdata <- x$Env$R
    drawdowns <- PerformanceAnalytics:::Drawdowns(xdata, geometric)
    chart.TimeSeries(drawdowns, ..., xaxis=FALSE, main="")
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
        names(list(geometric=geometric, ...)),
              list(geometric=geometric, ...))
  exp <- parse(text=gsub("list","plot_drawdowns",
               as.expression(substitute(list(x=current.chob(),
                                             geometric=geometric, ...)))),
               srcfile=NULL)
  plot_object <- current.chob()
  plot_object$add(exp, env=c(lenv, plot_object$Env), expr=TRUE)
  plot_object
}


