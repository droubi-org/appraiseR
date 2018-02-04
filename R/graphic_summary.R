#' Plot data frame graphical summaries
#'
#' \code{graphical summary} creates standardized boxplots for data frames visualizations.
#'
#' @param formula a generic formula.
#' @param df a dataframe.
#' @export
#' @examples
#' graphic_summary(valor ~ ., centro_2015@data)

graphic_summary <- function(formula, df) {
  mf <- stats::model.frame(formula = formula, data = df)

  predictors <- attr(stats::terms.formula(x = formula, data = df),
                     "term.labels")
  response <- colnames(mf)[attr(stats::terms.formula(x = formula, data = df),
                                "response")]

  parameters <- union(response, predictors)

  r <- length(parameters)
  par1 <- round(sqrt(r))
  par2 <- ceiling(r/par1)
  p <- list()

  for (i in parameters) {
    if (df %>% dplyr::select(i) %>% unlist() %>% is.factor)
      p[[i]] <- bboxplot.default(y = response, g = i, df = df)
    else p[[i]] <- bboxplot.default(y = i, df = df)

  }

  est <- list(plots = p,
              par1 = par1,
              par2 = par2,
              data = deparse(substitute(df))
  )
  class(est) <- "plotdf"
  est
}
