\name{StructTS.xts}

\alias{StructTS}
\alias{StructTS.default}
\alias{StructTS.xts}

\title{ StructTS for xts Objects }
\description{Fitting structural time series models for xts objects}
\usage{
\method{StructTS}{xts}(x, ...)
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{...}{ additional arguments to be passed to the \code{stats} function which drives these methods.}
}
\details{
  Currently little more than thin wrappers to \code{stats::StructTS} for xts objects. Will receive increased functionality going forward. See documentation of those functions for more information about the calculation mechanisms.
}
\value{A \code{StructTS} object with the residuals and fitted values coerced back to xts objects.}
\author{ Michael Weylandt }

\examples{
# Hopefully we don't break this
example("StructTS","stats")
}