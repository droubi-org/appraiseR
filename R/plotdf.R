#' Plot data frames
#' 
#' \code{plotdf} creates standardized plots for easy data frames visualizations.
#' 
#' @param formula a generic formula.
#' @param data a data frame.
#'   
#' @export
#' @examples
#' data <- as.data.frame(centro_2015@data)
#' plotdf(valor ~ . , data)

plotdf <- function(formula, data){
  mf <- stats::model.frame(formula = formula, data = data)
  
  predictors <- attr(stats::terms.formula(x = formula, data = data), "term.labels")
  response <-
    colnames(mf)[attr(stats::terms.formula(x = formula, data = data), "response")]
  parameters <- union(response, predictors)

  r <- length(parameters)
  par1 <- round(sqrt(r))
  par2 <- ceiling(r/par1)
  p <- list()

  for (i in parameters) {
    if (is.character(data[, i]) | is.factor(data[, i])) 
      p[[i]] <- bboxplot.default(y = response, g = i, data = data) 
    else
      p[[i]] <- bboxplot.default(y = i, data = data)
  }
                            
  est <- list(plots = p,
              par1 = par1,
              par2 = par2)
  class(est) <- "plotdf"
  est
}

#' @export
#'
print.plotdf <- function(x, ...){
  gridExtra::grid.arrange(grobs =x$plots, nrow =x$par1, ncol = x$par2)
}
