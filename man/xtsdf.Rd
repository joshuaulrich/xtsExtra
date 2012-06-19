\name{xtsdf}
\alias{xtsdf}
\alias{is.xtsdf}
\alias{as.xtsdf}
\title{ Create Or Test For An xtsdf Time-Series Object }
\description{
Constructor function for creating an extensible time-series data.frame object.

\code{xtsdf} is used to create an \code{xtsdf} object from raw data inputs and \code{as.xtsdf} is used for coercion to the same.
}
\usage{
xtsdf(..., order.by = index(x), frequency = NULL, unique = TRUE, tzone = Sys.getenv("TZ"), stringsAsFactors = default.stringsAsFactors(), check.names = TRUE)

as.xtsdf(x, ...)

is.xtsdf(x)
}

\arguments{
  \item{\dots}{ Columns of the resulting \code{xtsdf} object. Names will be extracted as with \code{data.frame}}
  \item{order.by}{ a corresponding vector of unique times/dates - must be
of a known time-based class. See details of \code{xts}.}
  \item{frequency}{ numeric indicating frequency of \code{order.by}. See details of \code{xts}. }
  \item{unique}{ should index be checked for unique time-stamps? }
  \item{tzone}{ time zone of series }
  \item{stringsAsFactors}{ Should strings be converted to factors?}
  \item{check.names}{Should column names be made syntactically correct?}
  \item{x}{An object to be tested or coerced to class \code{xtsdf}.}
}
\details{
An \code{xtsdf} object creates a time ordered \code{data.frame} like object; internally it is implemented as a list of \code{xts} objects, but it should behave like a \code{data.frame} rather transparently. The class is still under heavy development and feedback is welcome. Imporantly, this now allows \code{xts}-like behavior for objects with different column classes.



Subsetting inherits ISO8601 subsetting from \code{xts} as well as standard R subsetting. Double bracket (\code{[[}) subsetting inherits from the underlying list structure. Note that the default for subsetting is \code{drop = FALSE} following \code{xts} rather than \code{data.frame}. This is subject to change.

}
\value{
An S3 object of class \code{xtsdf}. 
}

\author{Michael Weylandt}

\seealso{ \code{help("xts")} }
\examples{
data(sample_matrix)
sample.xtsdf <- xtsdf(fac = letters[sample(26, 100, replace = TRUE)], x = rnorm(100), y = abs(rnorm(100)), order.by = as.Date("2012-06-19") + 1:100)

class(sample.xtsdf)
str(sample.xtsdf)

head(sample.xtsdf)  

sample.xtsdf['2012']  # all of 2012
sample.xtsdf['2007-07/']  # July 2012 to the end of the data set
sample.xtsdf['2007-07/2007-08']  # March 2007 to August of 2007
sample.xtsdf['/'] # the whole data set

sample.xtsdf[["x"]]

sample.xtsdf[,"fac"]
}
\keyword{ utilities }
