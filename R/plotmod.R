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
#' data <- centro_2015@data
#' fit <- lm(log(valor) ~ ., data = data)
#' plotvar(fit, "area_total", interval = "confidence")
#' plotvar(fit, "area_total", "log", interval = "confidence")
#' plotvar(fit, "padrao", interval = "confidence")
#' plotvar(fit, "padrao", "log", interval = "confidence")
#' 
plotvar <- function(object, variable, func,
                    interval = c("none", "confidence", "prediction"),
                    level = 0.80, ...){
  interval <- match.arg(interval)
  z <- object
  df <- eval(stats::getCall(z)$data)
  vars <- all.vars(stats::formula(z))
  params <- parameters(z)
  response <- params$response
  preds <- params$predictors
 
  df %<>% dplyr::as_tibble() %>% dplyr::mutate_if(is.character, as.factor)
  
  if (is.factor(dplyr::pull(df[, variable]))){
    grid <- levels(dplyr::pull(df[, variable]))
    new <- data.frame(grid, lapply(df[setdiff(preds, variable)], centre))
    names(new)[1] <- variable
    Y <- stats::predict.lm(object = z, newdata = new, interval = interval,
                           level = level, ...)
    if (!missing(func)) Y <- inverse(Y, func)
    pred <- data.frame(grid, Y)
    pred_plot <- reshape2::melt(pred, id.var = "grid")
    p <- ggplot(data = pred_plot, aes(factor(grid), value)) + 
      geom_boxplot(aes(fill = factor(grid))) +                
      xlab(variable) + ylab(response) +                           
      theme(legend.position="bottom")   
  } else {
    grid <- seq(min(df[, variable], na.rm = TRUE), max(df[, variable], na.rm = TRUE),
                length = 101)
    new <- data.frame(grid, lapply(df[setdiff(preds, variable)], centre))
    names(new)[1] <- variable
    Y <- stats::predict.lm(object = z, newdata = new, interval = interval,
                           level = level, ...)
    if (!missing(func)) Y <- inverse(Y, func)
    pred <- data.frame(grid, Y)
    p <- ggplot(data = pred, aes(x = grid, y = fit)) +
      geom_line() + 
      xlab(variable) + ylab(response) +                                           
      theme(legend.position="bottom")                                               
    if (interval != "none") {
      p <- p + geom_ribbon(aes(ymin = lwr, ymax = upr, colour = "grey", alpha = 0.5),
                           stat = "identity") +                                           
        theme(legend.position="none")       
    }
  }
  return(p)
}

teste <- function(object, variable, func,
                            interval = c("none", "confidence", "prediction"),
                            level = 0.80, ...){
        interval <- match.arg(interval)
        if (interval != "none") {
          return(interval)
        } else {
          return("ok")
        }
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
#' best_fit <- bestfit(valor ~ .,  data = data)
#' plotmod(best_fit, interval = "confidence")
#' plotmod(best_fit, fit = 2, interval = "confidence")
#' 
#' fit <- lm(log(valor) ~ ., data = data)
#' plotmod(fit, interval = "confidence")
#' plotmod(fit, interval = "confidence", func = "log")
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

#' @rdname plotmod
#' @export
plotmod.lm <- function(object, interval = c("none", "confidence", "prediction"), 
                       level = 0.80, 
                       func = "identity", ...){
  preds <- parameters(object)$predictors
  
  r <- length(preds)
  par1 <- round(sqrt(r))
  par2 <- ceiling((r)/par1)
  p <- list()
  
  for (i in preds) {
    p[[i]] <- plotvar(object = object, var = i, func = func, interval = interval)
  }
  
  est <- list(plots = p,
              par1 = par1,
              par2 = par2)
  class(est) <- "plotmod.lm"
  return(est)
}

#' @export
#'
print.plotmod.lm <- function(x, ...){
  gridExtra::grid.arrange(grobs =x$plots, nrow =x$par1, ncol = x$par2)
}

#' @export
#'
print.plotmod.bestfit <- function(x, ...){
  gridExtra::grid.arrange(grobs =x$plots, nrow =x$par1, ncol = x$par2)
}