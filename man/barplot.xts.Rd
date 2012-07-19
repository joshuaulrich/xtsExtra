\name{barplot.xts}
\alias{barplot.xts}
\title{Time-Oriented Barplots from xts Objects}
\description{A barplot method for xts objects.}
\usage{
\method{barplot}{xts}(height, stacked = TRUE, scale = FALSE, auto.legend = TRUE, 
                        major.format = TRUE, ylim=NULL, space = 0.2, cex.axis=0.8,
                        cex.legend = 0.8, cex.lab = 1, cex.labels = 0.8, cex.main = 1,
                        xaxis=TRUE, box.color = "black", xlab="Date",
                        ylab="Value", major.ticks='auto', minor.ticks=TRUE,
                        xaxis.labels = NULL, col = NULL, ...)
}
\arguments{
  \item{height}{An \code{xts} object of desired frequency. Use \code{to.period}
  functions to aggregate data outside of \code{barplot}.}
  \item{stacked}{logical. Should barplot be stacked?}
  \item{scale}{logical. Should data be rescaled to sum to 100\% at each bar? 
      Currently does not support negative values.}
  \item{auto.legend}{logical. Should a legend be placed underneath the barplot?}
  \item{major.format}{Format string for x-axis time labels. If \code{TRUE}, 
      automatically chosen}
  \item{ylim}{Limits of y-axis. If \code{NULL}, sensible defaults are chosen.}
  \item{space}{the amount of space (as a fraction of the
  average bar width) left before each bar, as in
  \code{\link{barplot}}. Default is 0.2.}
  
  \item{cex.axis}{The magnification to be used for sizing
  the axis text relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}
  
  \item{cex.legend}{The magnification to be used for sizing
  the legend relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{cex.labels}{The magnification to be used for event
  line labels relative to the current setting of 'cex'.}

  \item{cex.lab}{The magnification to be used for x- and
  y-axis labels relative to the current setting of 'cex'.}

  \item{cex.main}{The magnification to be used for the
  chart title relative to the current setting of 'cex'.}

  \item{xaxis}{If true, draws the x axis}
  
  \item{box.color}{provides the color for drawing
  less-important chart elements, such as the box lines,
  axis lines, etc.}

  \item{xlab}{the x-axis label, which defaults to 'NULL'.}

  \item{ylab}{Set the y-axis label, same as in
  \code{\link{plot}}}

  \item{major.ticks}{Should major tickmarks be drawn and
  labeled, default 'auto'}

  \item{minor.ticks}{Should minor tickmarks be drawn,
  default TRUE}
  
  \item{xaxis.labels}{Allows for non-date labeling of date
  axes, default is NULL}

  \item{col}{Color of bars. Defaults to a bright color pallette rather than the
  grays that are the default of other \code{baplot} methods.}

  \item{\dots}{ additional graphical arguments passed to \code{\link{barplot}}.}
}
\details{
Used to provide time-oriented barplots. 
}
\value{Invisibly returns the plotted object. Also assigns \code{.barplot.xts} to \code{.GlobalEnv} which can be used to "replay" the plot.}
\author{Peter Carl with modifications by Michael Weylandt}

\examples{
weights <- xts(matrix(runif(42, 0.1, 0.3), ncol = 6), as.yearmon(Sys.Date()) + 0:6)
names(weights) <- paste("Factor", LETTERS[1:6])

barplot(weights)

# No legend
barplot(weights, auto.legend = FALSE)

# Not Stacked
barplot(weights, stacked = FALSE)

# Scale weights to sum to 1
barplot(weights, scale = TRUE)

# A nicer color pallette
# rainbow6equal from Performance Analytics
barplot(weights, scale = TRUE, col = c("#BF4D4D", "#BFBF4D", "#4DBF4D", "#4DBFBF","#4D4DBF", "#BF4DBF"))

# Include negative data
weights[,3] <- weights[,3] * -1

barplot(weights)

barplot(weights, stacked = FALSE)
}
