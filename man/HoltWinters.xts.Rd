\name{HoltWinters.xts}

\alias{HoltWinters}
\alias{HoltWinters.default}
\alias{HoltWinters.xts}

\title{ HoltWinters for xts Objects }
\description{Holt-Winters Filtering for xts objects}
\usage{
\method{HoltWinters}{xts}(x, ...)
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{...}{ additional arguments to be passed to the \code{stats} function which drives these methods.}
}
\details{
  Currently little more than thin wrappers to \code{stats::HoltWinters} for xts objects. Will receive increased functionality going forward. See documentation of those functions for more information about the calculation mechanisms.
}
\value{A \code{HoltWinters} object with the residuals and fitted values coerced back to xts objects.}
\author{ Michael Weylandt }

\examples{
# Hopefully we don't break this
example("HoltWinters","stats")
}