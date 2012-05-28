\name{barplot.xts}
\alias{barplot.xts}
\title{ Time-Oriented Barplots from xts Objects }
\description{A barplot method for xts objects.}
\usage{
\method{barplot}{xts}(height, stacked = TRUE, scale = FALSE, ...)
}
\arguments{
  \item{height}{ an \code{xts} object of desired frequency. Use \code{to.period} functions to aggregate data outside of \code{barplot}.}
  \item{stacked}{logical. Should barplot be stacked?}
  \item{scale}{logical. Should data be rescaled to sum to 100\% at each bar? Currently does not support negative values.}
  \item{\dots}{ additional graphical arguments }
}
\details{
Used to provide time-oriented barplots. 
}
\value{Invisibly returns the plotted object. Also assigns \code{.barplot.xts} to \code{.GlobalEnv} which can be used to "replay" the plot.}
\author{ Michael Weylandt using code by P. Carl and B. Peterson}