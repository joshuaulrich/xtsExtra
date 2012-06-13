\name{acf.xts}
\alias{acf.xts}
\alias{pacf.xts}
\alias{acf}
\alias{acf.default}
\alias{acf.ts}
\title{ Autocorrelation for xts Objects }
\description{(Partial) autocorrelation functions for xts objects}
\usage{
\method{acf}{xts}(x, ...)
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{...}{ additional arguments to be passed to the \code{stats} function which drives these methods.}
}
\details{
  Currently little more than thin wrappers to \code{stats::acf} and \code{stats::pacf.default} for xts objects. Will receive increased functionality going forward. See documentation of those functions for more information about the calculation mechanisms.
}
\value{An \code{acf} or \code{pacf} object as appropriate.}
\author{ Michael Weylandt }

\examples{
# Hopefully we don't break this
example("acf", "stats")
example("pacf", "stats")
}