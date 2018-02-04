#' Beautiful boxplots with ggplot2
#' 
#' @param y a numeric variable for which the boxplot is to be constructed.
#' @param g a grouping variable, usually a factor, for constructing parallel
#'   boxplots.
#' @param data a dataframe containing the variables.
#'   
#' @return A boxplot of the variable \code{y} or the boxplot of the \code{y}
#'   against \code{g}.
#' @export
#' 
#' @examples
#' bboxplot("Valor_Total", data = centro_2015)
#' bboxplot("Valor_Total", "Padrao", centro_2015)
#' 
bboxplot <- function(y, g, data) UseMethod("bboxplot")

#' @rdname bboxplot
#' @export

bboxplot.default <- function(y, g, data){
  data$id <- rownames(data)
  if (missing(g)) {
    out <- grDevices::boxplot.stats(data[, y])$out
    if (length(out) == 0) {
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = 1, y = y)) +
        ggplot2::geom_boxplot(fill = "grey80") + ggplot2::geom_rug() +
        ggplot2::xlab("")
    } else {
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = 1, y = y)) +
        ggplot2::geom_boxplot(fill = "grey80", outlier.colour = "red") + 
        ggplot2::geom_text(data = data[match(out, data[,y]),],
                           ggplot2::aes_string(x = 1, y = y, label = "id"),
                           hjust=-1, size=4) + 
        ggplot2::geom_rug() + ggplot2::xlab("")
    }
  } else {
    if (is.character(data[, g])) data[, g] <- as.factor(data[, g])
    out <- grDevices::boxplot.stats(data[, y])
    p <- ggplot2::ggplot(data,
                         ggplot2::aes_string(x = data[, g], y = y)) +
      ggplot2::xlab(g) + 
      ggplot2::geom_boxplot(ggplot2::aes_string(fill = g),
                            outlier.colour = "red") +
      ggplot2::geom_rug() + ggplot2::theme(legend.position="none")
  }
  return(p)
}

#' @param formula A ‘model’ formula, of the form ~ y to produce a boxplot for
#'   the variable y, or of the form y ~ g to produce parallel boxplots for y
#'   within levels of the grouping variable(s) g, etc., usually factors.
#' @param data A data frame containing the variables in the formula.
#' @examples
#' bboxplot(Valor_Total~Padrao, centro_2015)
#' bboxplot(~Valor_Total, centro_2015)

#' @rdname bboxplot
#' @export bboxplot.formula
#'   
bboxplot.formula <- function(formula, data) {
  response <- attr(stats::terms.formula(formula, data = data),
                                "response")
  var <- attr(terms.formula(formula, data = data), "term.labels")
  
  if (response == 0) p <- bboxplot.default(y = var, data = data) else {
    response <- colnames(data)[response]
    if (is.character(data[, var])) data[, var] <- as.factor(data[, var])
    p <- bboxplot.default(y = response, g = var, data = data)
  }
  return(p)
}

#' Plot data frames
#' 
#' \code{plotdf} creates standardized plots for easy data frames visualizations.
#' 
#' @param formula a generic formula.
#' @param data a data frame.
#'   
#' @export
#' @examples
#' plotdf(Valor_Total ~ . - lat - lon, centro_2015)

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

  for (i in parameters) if (is.character(data[, i]) | is.factor(data[, i])) 
    p[[i]] <- bboxplot.default(y = response, g = i, data = data) else
      p[[i]] <- bboxplot.default(y = i, data = data)
                            
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
