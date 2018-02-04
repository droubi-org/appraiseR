#' Plot model predictors against the response variable with intervals
#' 
#' \code{plotvar} plots model variables with confidence/prediction intervals
#' @param object object of class lm
#' @param var variable to be plotted against response variable
#' @param func function used to transform the response (optional)
#' @param \dots further arguments passed to predict.lm.
#' @param interval the type of interval calculation (provided to predict.lm) to
#'   be ploted.
#' @param level Tolerance/confidence level (provided to predict.lm) to
#'   be ploted.
#' @export
#' @examples
#' fit <- lm(log(Valor_Total) ~ . - lat - lon, centro_2015)
#' plotvar(fit, "Area_Total", interval = "confidence")
#' plotvar(fit, "Area_Total", "log", interval = "confidence")
#' plotvar(fit, "Padrao", interval = "confidence")
#' plotvar(fit, "Padrao", "log", interval = "confidence")
#' 
plotvar <- function(object, var, func,
                    interval = c("none", "confidence", "prediction"),
                    level = 0.80, ...){
  z <- object
  data <- eval(stats::getCall(z)$data)
  vars <- all.vars(stats::formula(z))
  params <- parameters(z)
  response <- params$response
  preds <- setdiff(vars, response)
 
  for (i in preds) if (is.character(data[,i])) data[,i] <- as.factor(data[,i])
  
  if (is.factor(data[,var])){
    grid <- levels(data[,var])
    new <- data.frame(grid, lapply(data[setdiff(preds, var)], REATOOLS::centre))
    names(new)[1] <- var
    Y <- stats::predict.lm(object = z, newdata = new, interval = interval,
                           level = level, ...)
    if (!missing(func)) Y <- inverse(Y, func)
    pred <- data.frame(grid, Y)
    pred_plot <- reshape2::melt(pred, id.var = "grid")
    p <- ggplot2::ggplot(data = pred_plot, ggplot2::aes(factor(grid), value)) + 
      ggplot2::geom_boxplot(ggplot2::aes(fill = factor(grid))) +                
      ggplot2::xlab(var) + ggplot2::ylab(response) +                           
      ggplot2::theme(legend.position="bottom")   
  } else {
    grid <- seq(min(data[,var], na.rm = TRUE), max(data[,var], na.rm = TRUE),
                length = 101)
    new <- data.frame(grid, lapply(data[setdiff(preds, var)], REATOOLS::centre))
    names(new)[1] <- var
    Y <- stats::predict.lm(object = z, newdata = new, interval = interval,
                           level = level, ...)
    if (!missing(func)) Y <- inverse(Y, func)
    pred <- data.frame(grid, Y)
    p <- ggplot2::ggplot(data = pred, ggplot2::aes(x = grid, y = fit)) +
      ggplot2::geom_line() + 
      ggplot2::xlab(var) + ggplot2::ylab(response) +                                           
      ggplot2::theme(legend.position="bottom")                                               
    if (!missing(interval))
      p <- p + ggplot2::geom_smooth(ggplot2::aes(ymin = lwr, ymax = upr),
                                    stat = "identity")
  }
  return(p)
}


#' Plot bestfit models
#' 
#' \code{plotmod} plots \code{\link{bestfit}} models
#' 
#' @param object Object of class \code{\link{bestfit}}
#' @param fit the number of the chosen fit from the combination matrix (defaults
#'   for the best fit found with \code{\link{bestfit}}).
#' @param interval the type of interval calculation (provided to predict.lm) to 
#'   be ploted with the model.
#' @param \dots further arguments passed to predict.lm.
#' @param level Tolerance/confidence level (provided to predict.lm) to be
#'   ploted.
#' @export
#' @examples
#' best_fit <- bestfit(Valor_Total ~ . - lat - lon, centro_2015)
#' plotmod(best_fit, interval = "confidence")
#' plotmod(best_fit, fit = 2, interval = "confidence")
#' 

plotmod <- function(object, ...) UseMethod("plotmod")

#' @rdname plotmod
#' @export

plotmod.bestfit <- function(object, fit = 1, 
                            interval = c("none", "confidence", "prediction"),
                            level = 0.80, ...){
  s <- summary(object, fit = fit)
  z <- s$fit
  func <- as.character(s$bestfit[, object$response])
  terms <- object$predictors

  r <- length(terms)
  par1 <- round(sqrt(r))
  par2 <- ceiling((r)/par1)
  p <- list()

  for (i in 1:length(terms)) p[[i]] <- plotvar(object = z, var = terms[i],
                                               func = func, interval = interval)

  est <- list(plots = p,
              par1 = par1,
              par2 = par2)
  class(est) <- "plotmod.bestfit"
  return(est)
}

#' @export
#'
print.plotmod.bestfit <- function(x, ...){
  gridExtra::grid.arrange(grobs =x$plots, nrow =x$par1, ncol = x$par2)
}
