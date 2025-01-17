% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sref.R
\name{sslice}
\alias{sslice}
\title{Create a Safer Reference Slice to a Vector}
\usage{
sslice(x, ...)
}
\arguments{
\item{x}{object to be referenced; must be a symbol or character}

\item{...}{objects passed to \code{x[...]} when dereferenced}
}
\value{
object of class \code{"sslice"} and \code{"sref"}
}
\description{
Create a reference to a 'part' of an R object. \code{sslice} behaves similar to \code{\link{slice}}, but does not
have support for direct operations on the referenced object. See \code{\link{sref}} for details about the behavior.
}
