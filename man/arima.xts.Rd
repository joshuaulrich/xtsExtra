\name{arima.xts}

\alias{arima}
\alias{arima.default}
\alias{arima.xts}

\alias{arima0}
\alias{arima0.default}
\alias{arima0.xts}


\title{ ARIMA for xts Objects }
\description{Autoregressive Integrated Moving Average models for xts objects}
\usage{
\method{arima}{xts}(x, ...)
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{...}{ additional arguments to be passed to the \code{stats} function which drives these methods.}
}
\details{
  Currently little more than thin wrappers to \code{stats::arima} and \code{stats::arima0} for xts objects. Will receive increased functionality going forward. See documentation of those functions for more information about the calculation mechanisms.
}
\value{An \code{Arima} or \code{arima0} object as appropriate with the residuals coerced back to xts objects.}
\author{ Michael Weylandt }

\examples{
# Hopefully we don't break this
example("arima","stats")
example("arima0","stats")
}