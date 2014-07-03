

# Environment for our xts chart objects
.plotxtsEnv <- new.env()

new.chob <- function(frame=1, xlim=c(1,10), ylim=list(structure(c(1,10), fixed=FALSE))){
  # This function is modeled after quantmod::new.replot
  Env <- new.env()
  
  # Not exactly sure what frame is doing
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
  set_frame <- function(frame) { Env$frame <<- frame }
  # set_asp <- function(asp) { Env$asp <<- asp }
  set_xlim <- function(xlim) { Env$xlim <<- xlim }
  set_ylim <- function(ylim) { Env$ylim <<- ylim }
  set_pad <- function(pad) { Env$pad1 <<- pad[1]; Env$pad3 <<- pad[2] }
  
  ##### getters #####
  get_frame <- function(frame) { Env$frame }
  # get_asp   <- function(asp) { Env$asp }
  get_xlim  <- function(xlim) { Env$xlim }
  get_ylim  <- function(ylim) { Env$ylim }
  get_pad   <- function() c(Env$pad1,Env$pad3)
  
  # panels is a list where each element (i.e. slot) is what we want to evaluate
  Env$panels <- list()
  
  # add an expression to Env$panels (i.e. Env$actions in quantmod)
  add <- function(x, env=Env, expr=FALSE, ...) {
    if(!expr) {
      x <- match.call()$x
    }
    # each element in the Env$panels list is an object with "frame" and "env"
    # as environments
    a <- structure(x, frame=Env$frame, env=env, ...)
    Env$panels[[length(Env$panels)+1]] <<- a
  }
  
  # create a new environment that contains Env as one of its elements
  plotxts_env <- new.env()
  class(plotxts_env) <- c("plotxts", "environment")  
  plotxts_env$Env <- Env
  
  # add the setters to the plotxts_env environment
  plotxts_env$set_frame <- set_frame
  # plotxts_env$set_asp <- set_asp
  plotxts_env$set_xlim <- set_xlim
  plotxts_env$set_ylim <- set_ylim
  plotxts_env$set_pad <- set_pad
  
  # add the getters to the plotxts_env environment
  plotxts_env$get_frame <- get_frame
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
plot2_xts <- function(R, byColumn=FALSE, ...){
  # this function is modeled after quantmod::chart_Series
  # initialize a new chart object
  cs <- new.chob()
  
  # Env$R will hold the original returns object passed in
  cs$Env$R <- R
  cs$Env$byColumn <- byColumn
  
  cs$set_xlim(c(1, NROW(cs$Env$R)))
  cs$set_ylim(list(structure(range(na.omit(cs$Env$R)),fixed=FALSE)))
  
  # We should also do stuff here to get a common x-axis to use for each panel
  # or chart to work with specifying multiples
  # cs$set_xaxis()
  
  cs$set_frame(1)
  # Default plot behavior
  # Can we just call chart.TimeSeries like this?
  # This is a temporary workaround for this prototype
  cs$Env$.formals <- formals(chart.TimeSeries)
  cs$Env$.formals <- modify.args(cs$Env$.formals, arglist=list(...), dots=TRUE)
  cs$Env$.formals$`...` <- NULL
  assign(".xts_chob", cs, .plotxtsEnv)
  cs
}

# print/plot
print.plotxts <- function(x, ...) plot.plotxts(x,...)
plot.plotxts <- function(x, ...){
  
  # Restore old par() options from what I change in here
  old.par <- par()
  on.exit(par(old.par))
  
  plot.new()
  
  # Here we assign x to the .plotxtsEnv
  # x should have all of the data we need for plotting, layouts, etc
  assign(".xts_chob", x, .plotxtsEnv)
  
  .formals <- x$Env$.formals
  R <- x$Env$R
  pad1 <- x$Env$pad1
  pad3 <- x$Env$pad3
  
  # This is an ugly hack to get the basic prototype working
  if(isTRUE(x$Env$byColumn)){
    layout(matrix(seq.int(from=1, to=NCOL(R), by=1L)), widths=1, heights=1)
    .formals$xaxis <- FALSE
    .formals$main <- ""
    .formals$ylim <- x$Env$ylim[[1]]
    for(i in 1:NCOL(R)){
      if(i == 1){
        # 0 margin on the bottom
        par(mar=c(pad1, 4, 4, 2))
      } else if(i == NCOL(R)){
        par(mar=c(5, 4, pad3, 2))
      } else {
        # 0 margin on the top and bottom
        par(mar=c(pad1, 4, pad3, 2))
      }
      .formals <- modify.args(.formals, R=R[,i], dots=TRUE)
      do.call(chart.TimeSeries, .formals)
    }
    ep <- xtsExtra:::axTicksByTime(R)
    cex.axis = 1
    label.height = cex.axis *(.5 + apply(t(names(ep)),1, function(X) max(strheight(X, units="in")/par('cin')[2]) ))
    xaxis.labels = names(ep)
    axis(1, at=ep, labels=xaxis.labels, las=1, lwd=1, mgp=c(3,label.height,0))
  } else {
    .formals <- modify.args(.formals, R=R, dots=TRUE)
    do.call(chart.TimeSeries, .formals)
  }
  
  # Evaluate the expression in the Env$panels list
  npanels <- length(x$Env$panels)
  if(npanels > 0){
    for(i in 1:npanels){
      env <- attr(x$Env$panels[[i]], "env")
      eval(x$Env$panels[[i]], env)
    }
  }
}


