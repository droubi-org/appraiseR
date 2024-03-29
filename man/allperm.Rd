% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allperm.R
\name{allperm}
\alias{allperm}
\title{Computes all the combinations of the transformed parameters}
\usage{
allperm(data, subset, select = colnames(data), transf = c("rsqrt",
  "log", "sqrt"))
}
\arguments{
\item{data}{A data frame}

\item{subset}{a specification of the rows to be used: defaults to all rows.
This can be any valid indexing vector (see \link{[.data.frame}) for the
rows of data or if that is not supplied, a data frame made up of the
variables used in \code{formula}.}

\item{select}{a character vector containing selected colnames from a data frame.
Defaults for all the variables in data. See \link{subset}.}

\item{transf}{The transformations to be applied to each parameter}
}
\value{
A matrix with all the working combinations of supplied
  transformations
}
\description{
Computes all the combinations of the transformed parameters
}
\examples{

vars <- colnames(centro_2015@data)
perms <- allperm(centro_2015@data, select = vars)

vars <- colnames(itacorubi_2015@data)
perms <- allperm(itacorubi_2015@data, select = vars)
}
