% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/incr.R
\name{\%-=\%}
\alias{\%-=\%}
\title{Subtract In Place}
\usage{
x \%-=\% value
}
\arguments{
\item{x}{object to be modified; can be a symbol, character, or extraction language object.}

\item{value}{value with which to change \code{x} by}
}
\value{
the new value of \code{x}, invisibly
}
\description{
Decrease the value of an object on the search path. Equivalent to \code{'-='} in other languages.
See \code{\link{incr}} for details on implementation.
}
\examples{
x <- 11:20
x \%-=\% 10
identical(x, 1:10)  # TRUE
}
