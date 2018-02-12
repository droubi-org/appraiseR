#' Plot data frame graphical summaries
#'
#' \code{graphical summary} creates standardized boxplots for data frames visualizations.
#'
#' @param formula a generic formula.
#' @param data a dataframe.
#' @export
#' @examples
#' data <- as.data.frame(centro_2015@data)
#' graphic_summary(valor ~ ., data = data)

graphic_summary <- function(formula, data) {
  mf <- stats::model.frame(formula = formula, data = data)

  predictors <- attr(stats::terms.formula(x = formula, data = data),
                     "term.labels")
  response <- colnames(mf)[attr(stats::terms.formula(x = formula, data = data),
                                "response")]

  parameters <- union(response, predictors)

  r <- length(parameters)
  par1 <- round(sqrt(r))
  par2 <- ceiling(r/par1)
  p <- list()

  for (i in parameters) {
    if (is.factor(data[,i]))
      p[[i]] <- bboxplot.default(y = response, g = i, data = data)
    else p[[i]] <- bboxplot.default(y = i, data = data)

  }

  est <- list(plots = p,
              par1 = par1,
              par2 = par2,
              data = deparse(substitute(df))
  )
  class(est) <- "plotdf"
  est
}
