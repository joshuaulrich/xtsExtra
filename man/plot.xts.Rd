\name{plot.xts}
\alias{plot.xts}
\title{ Plotting xts Objects }
\description{
Plotting methods for xts objects.
}
\usage{
\method{plot}{xts}(x, y = NULL, 
            screens = 'auto', layout.screens = 'auto',
            yax.loc = c("none","out","in","flip", "left", "right", "top"),
            auto.grid=TRUE, major.ticks='auto', minor.ticks=TRUE, 
            major.format=TRUE, bar.col.up = 'white',
            bar.col.dn ='red', candle.col='black',
            xy.labels = FALSE, xy.lines = NULL, 
            ylim = 'auto', panel = lines, 
            auto.legend = FALSE, legend.names = colnames(x), 
            legend.loc = "topleft", legend.pars = NULL, 
            events, blocks, nc, nr, ...)
}
\arguments{
  \item{x}{an \code{xts} object}
  \item{y}{an \code{xts} object or \code{NULL}}
  \item{screens}{factor (or coerced to factor) whose levels specify which graph each series is to be plotted in. If not specified, then defaults to a single series per screen for \code{type} not \code{"candles"} or \code{"bars"} See examples.}
  \item{layout.screens}{ Matrix (in a form that could be passed to layout) which arranges screens.}
  \item{yax.loc}{Where to put y-axis labels if they are to be drawn? \code{'none'}, \code{'left'},\code{'right'} are rather self-evident. \code{'in'} and \code{'out'} currently only work for two column layouts. \code{'flip'} attemps to arrange the axes to alternate between the left and right hand sides of the plot to avoid label collision. \code{'top'} is something of a misnomer: it places the y-axis labels above each panel while leaving all plots with their own x- and y-axes. 
  }
  \item{auto.grid}{should grid lines be drawn}
  \item{major.ticks}{should major tickmarks be drawn and labeled}
  \item{minor.ticks}{should minor tickmarks be drawn}
  \item{major.format}{passed along to axTicksByTime.}
  \item{bar.col.dn}{the color of the \dQuote{down} bars when \code{type} is \sQuote{bars} or \sQuote{candles}}
  \item{bar.col.up}{the color of the \dQuote{up} bars when \code{type} is \sQuote{bars} or \sQuote{candles}}
  \item{candle.col}{the color of the candles when \code{type} is \sQuote{candles}. Also the outside of the bars.}
  \item{xy.labels}{label points in scatterplot?}
  \item{xy.lines}{connect points in scatterplot?}
  \item{ylim}{How to handle \code{ylim} for plots. If \code{'fixed'} all panels share \code{ylim = range(x)}; if \code{'auto'} panels sharing a y axis have the same limits. If a numeric matrix, rows are recycled panel-wise as \code{ylim}.}
  \item{panel}{A panel function for plotting; by default, \code{lines.xts}. Currently, is passed \code{col}, \code{pch}, \code{type}, \code{lwd}, \code{cex} by default so be prepared to handle these arguments, perhaps by receiving them via \code{...} and ignoring. If \code{panel != lines}, that is, if the user supplies a panel function, the first two arguments passed will be \code{as.POSIXct(index(z))} and \code{coredata(z)} where \code{z} is the series being plotted in that panel; as a result, note that any plotting inside \code{panel} requires \code{POSIXct}, regardless of the index class of \code{x}. Note further that \code{panel} is called for each series individually, even if they share a screen. If a list of panel functions is passed, they are recycled panelwise (not serieswise).}
  \item{auto.legend}{Should a legend be added automatically?}
  \item{legend.names}{Names of series to be passed to the legend writing code. Should be one name per series plotted.}
  \item{legend.loc}{Location of legend: will be recycled panelwise. If \code{NA}, a legend is ommitted for that panel}
  \item{legend.pars}{Additional arguments, passed as a list, to be passed to \code{legend}. Will not recycle}
  \item{events}{A list with mandatory elements \code{time} and \code{label} and optional 
  elements \code{col} and \code{lty} giving the events to be highlighted. See examples.}
  \item{blocks}{A list with mandatory elements \code{start.time} and \code{end.time} and 
  optional element \code{col} giving time-blocks to be highlighted. See examples.}
  \item{nc}{Number of columns to be used if \code{layout} is set to \code{'auto'}.}
  \item{nr}{Number of rows to be used if \code{layout} is set to \code{'auto'}.}
  \item{\dots}{additional graphical arguments}
}
\details{
Mainly used to draw time-series plots with sensible x-axis labels, it
can also plot basic OHLC series using \code{type='candles'} or \code{type='bars'} [\code{'bars'} does not seem to be currently supported].

Care has been taken to make plotting arguments passed by \code{...} behave smartly by classifying them as 'global', 'screen-wise', or 'column-wise' arguments and treating them as such. A partial listing includes:
\itemize{
  \item Global parameters: \code{main}
  \item Screen-wise parameters: \code{log}, \code{ylab}
  \item Column-wise parameters: \code{col},\code{type},\code{lwd}, \code{pch}, \code{cex}, \code{lty} 
}
Arguments that are not set to recycle are applied to all the plots via \code{par} internally; \code{par} is reset to the user's previous state upon exiting the function. 

Better financial plots can be found in the \pkg{quantmod} package, though
these are generally incompatible with standard R graphics tools.
}
\value{Invisibly returns the plotted object (or the merged object in the scatterplot case). Also assigns \code{.plot.xts} to \code{.GlobalEnv} which can be used to "replay" the plot.}
\author{ Jeffrey A. Ryan with extensions by Michael Weylandt }
\examples{
data(sample_matrix, package = "xts")
sample_xts <- as.xts(sample_matrix)

# A very basic time series plot
plot(sample_xts[,1]) 

# A candle chart. quantmod does this much better
plot(sample_xts, type='candles') 

plot(sample_xts[1:30, ], type = "candles")

plot(sample_xts[1:30, ], type = "candles", bar.col.up = "blue", bar.col.dn = "violet", candle.col = "green4")

# By default prefers panels
plot(sample_xts[,1:2]) 

# But this can be too much for many-columned data
plot(sample_xts[,rep(1:4, each = 3)]) 

# Can customize screen layout
plot(sample_xts, layout.screens = matrix(1:4, ncol = 2)) 

# Or even be fancy with it
plot(sample_xts[,1:3], layout.screens = matrix(c(1,1,2,3),ncol = 2, byrow = TRUE))

plot(sample_xts[,1:4], layout.screens = matrix(c(1,1,1,1,2,3,4,4),ncol = 2, byrow = TRUE))

# Or assign multiple series per screen (screens gets recycled as necessary)
# Note smart assignment of colors
plot(sample_xts, screens = 1:2) 

# Example of how columnwise args are treated 
# Note here that col gets recycled alongside screens so it's not helpful here
plot(sample_xts, screens = 1:2, col = 1:2)

# Best practice is to be specific in setting arguments and not use recycling
plot(sample_xts, screens = c(1,2,1,2), col = c(1,3,2,2))

# Example of how panelwise args are treated
plot(10^sample_xts, screens = 1:2, log= c("","y"))

# Global arguments play nice as well
plot(sample_xts[,1:3], main = "A Truly Remarkble Time Series Plot")

# Type = "h" nicely illustrates independent points
plot(sample_xts[1:75,1:2] - 50.5, type = c("l","h"), lwd = c(1,2))

# Arrangement of axis-labels
plot(sample_xts[,1:3], layout = matrix(c(1,1,2,3), ncol = 2, byrow = TRUE), yax.loc = "none")
plot(sample_xts[,1:3], layout = matrix(c(1,1,2,3), ncol = 2, byrow = TRUE), yax.loc = "left")
plot(sample_xts[,1:3], layout = matrix(c(1,1,2,3), ncol = 2, byrow = TRUE), yax.loc = "right")

plot(sample_xts, layout = matrix(1:4, ncol = 2), yax.loc = "in")
plot(sample_xts, layout = matrix(1:4, ncol = 2), yax.loc = "out")
plot(sample_xts, layout = matrix(1:4, ncol = 2), yax.loc = "flip")

plot(sample_xts[,c(1:4, 3:4)], layout = matrix(c(1,1,1,1,2,2,3,4,5,6), ncol = 2, byrow = TRUE), yax.loc = "flip")

plot(sample_xts[,c(1:4, 3:4)], layout = matrix(c(1,1,1,1,2,2,3,4,5,6), ncol = 2, byrow = TRUE), yax.loc = "left")

plot(sample_xts[,c(1:4, 3:4)], layout = matrix(c(1,1,1,1,2,2,3,4,5,6), ncol = 2, byrow = TRUE), yax.loc = "top")


# Type = "p" allows for transparent handling of the pch= argument
x <- xts(1:5, Sys.Date() + 1:5)
plot(x, type = "p")
plot(x, type = "p", pch = 1:5, col = 1:5)

# Multivariate recycles over columns
plot(merge(x,x), type = "p", pch = 1:5, col = 1:2)

# Unless you use a list
plot(merge(x,x), type = "p", pch = list(1:5), col = 1:2)
plot(merge(x,x,x), type = "p", pch = list(1:5, 1:3), col = 1:3)

# Same for col= and cex= arguments
plot(x, type = "p", col = 1:5, pch = 1:5, cex = 1:5)
plot(merge(x,x), type = "p", col = 1:5, pch = 1:5, cex = 1:5)
plot(merge(x,x), type = "p", col = list(1:5, 1:3), pch = list(1:5, 1:3), cex = list(1:5, 1:3))

# Using the events argument
plot(sample_xts[,1], events = list(time = c("2007-03-15", "2007-05-01"), label = "bad days"))

plot(sample_xts[,1], events = list(time = c("2007-03-15","2007-05-01"), label = "bad days"), blocks = list(start.time = c("2007-03-05", "2007-04-15"), end.time = c("2007-03-20","2007-05-30"), col = c("lightblue", "lightgreen")))

plot(sample_xts, layout.screens = matrix(c(1,1,2,3,4,4), ncol = 2, byrow = TRUE), events = list(time = c("2007-03-15","2007-05-01"), label = "bad days"), blocks = list(start.time = c("2007-03-05", "2007-04-15"), end.time = c("2007-03-20","2007-05-30"), col = c("lightblue", "lightgreen")))

# Makes a scatterplot if we pass two series
plot(sample_xts[,1],sample_xts[,2])

# With some options
plot(sample_xts[1:100,1],sample_xts[1:100,2], xy.labels = TRUE)

# Use of color gradient
cr <- colorRampPalette(c("#00FF00","#FF0000"))
plot(sample_xts[,1],sample_xts[,2], xy.labels = FALSE, xy.lines = TRUE, col = cr(NROW(sample_xts)), type = "l")

# Can also use non-xts time series classes if called explicitly
tser <- ts(cumsum(rnorm(50, 0.05, 0.15)), start = 2007, frequency = 12)
plot(tser)
plot.xts(tser)

tser <- cbind(tser, ts(cumsum(rnorm(50, 0.05, 0.15)), start = 2007, frequency = 12))
plot(tser)
plot.xts(tser)


\dontrun{
# Some of these will fail because they try to access the 
# parent frame in ways not congruent with plot.xts
plot.zoo <- plot.xts
example(plot.zoo)
rm(plot.zoo)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
