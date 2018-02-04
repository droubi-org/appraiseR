#' Power plots with ggplot2
#'
#' \code{power_plot} generates Power Plots for \code{\link{lm}} or
#' \code{\link{bestfit}} objects with \code{\link{ggplot2}}.
#'
#' @param object object of class \code{\link{lm}} or \code{\link{bestfit}}
#' @param \dots not used.
#' @return a power plot
#' @name power_plot
#' @export
power_plot <- function(object, ...) {
    UseMethod("power_plot")
}

#' @rdname power_plot
#' @examples
#' fit <- lm(log(valor) ~ ., centro_2015@data)
#' power_plot(fit)
#' @export

power_plot.lm <- function(object, ...) {
  z <- object
  attr(z$terms, "variables")
  data <- stats::model.frame(z)


  Y <- data[, attr(z$terms, "response")]
  Y_ajustado <- z$fitted.values
  invres <- data.frame(Y, Y_ajustado)
  p <- ggplot2::ggplot(data = invres, ggplot2::aes(x = Y,
                                                   y = Y_ajustado)) + ggplot2::geom_point() +
    ggplot2::ylab(latex2exp::TeX("$\\hat{Y}$")) +
    ggplot2::geom_abline()
  p
}

#' @rdname power_plot
#' @param fit chosen fit
#' @examples
#' best_fit <- bestfit(valor ~ ., centro_2015@data)
#' power_plot(best_fit, fit = 257)
#' @export

power_plot.bestfit <- function(object, fit = 1, ...) {
    s <- summary(object, fit = fit)
    z <- s$fit

    p <- power_plot.lm(z)
    p
}
