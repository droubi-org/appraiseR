% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotdf.R
\name{plotdf}
\alias{plotdf}
\title{Plot data frames}
\usage{
plotdf(formula, data)
}
\arguments{
\item{formula}{a generic formula.}

\item{data}{a data frame.}
}
\description{
\code{plotdf} creates standardized plots for easy data frames visualizations.
}
\examples{
data <- as.data.frame(centro_2015@data)
plotdf(valor ~ . , data)
}
