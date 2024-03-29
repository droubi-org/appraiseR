% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/power_plot.R
\name{power_plot}
\alias{power_plot}
\alias{power_plot.lm}
\alias{power_plot.bestfit}
\title{Power plots with ggplot2}
\usage{
power_plot(object, ...)

\method{power_plot}{lm}(object, ...)

\method{power_plot}{bestfit}(object, fit = 1, ...)
}
\arguments{
\item{object}{object of class \code{\link{lm}} or \code{\link{bestfit}}}

\item{\dots}{not used.}

\item{fit}{chosen fit}
}
\value{
a power plot
}
\description{
\code{power_plot} generates Power Plots for \code{\link{lm}} or
\code{\link{bestfit}} objects with \code{\link{ggplot2}}.
}
\examples{
fit <- lm(log(valor) ~ ., centro_2015@data)
power_plot(fit)
best_fit <- bestfit(valor ~ ., centro_2015@data)
power_plot(best_fit, fit = 257)
}
