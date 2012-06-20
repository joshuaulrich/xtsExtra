\name{stl.xts}

\alias{stl}
\alias{stl.default}
\alias{stl.xts}

\title{ Loess Seasonal Decomposition for xts Objects }
\description{Decompose a time series into seasonal, trend, and irregular components using \code{loess}.}
\usage{
  \method{stl}{xts}(x, ...)
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{...}{ additional arguments to be passed to the \code{stats} function which drives these methods.}
}
\details{
  Currently little more than thin wrappers to \code{stats::stl} for \code{xts} objects. Will receive increased functionality going forward. See documentation of those functions for more information about the calculation mechanisms.
}
\value{A \code{stl} object with the residuals and fitted values coerced back to \code{xts} objects.}
\author{ Michael Weylandt }

\examples{
  # Hopefully we don't break this
  example("stl","stats")
}