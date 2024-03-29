% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paste.R
\name{paste_median}
\alias{paste_median}
\title{Paste median}
\usage{
paste_median(x, less.than.one = FALSE, digits = 1)
}
\arguments{
\item{x}{A numeric. Data to summarize.}

\item{less.than.one}{A logical. Indicates a median that rounds to 0 should
be printed as <1.}

\item{digits}{An integer. Number of digits to round to.}
}
\value{
A character vector of the median(s) with interquartile range(s).
}
\description{
Creates a formatted median with inter-quartile range from numeric data.
}
\examples{
paste_median(mtcars$mpg)
}
