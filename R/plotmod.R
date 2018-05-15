#' Plot model predictors against the response variable with intervals
#' 
#' \code{plotvar} plots model variables with confidence/prediction intervals
#' @param object object of class lm
#' @param variable variable to be plotted against response variable
#' @param func function used to transform the response (optional)
#' @param interval the type of interval calculation (provided to predict.lm) to
#'   be ploted.
#' @param level Tolerance/confidence level (provided to predict.lm) to
#'   be ploted.
#' @param local Data Frame to be used for calculate the estimates (defaults for center of each variable).
#' @param \dots further arguments passed to predict.lm.
#' @export
#' @examples
#' data <- centro_2015@data
#' fit <- lm(log(valor) ~ ., data = data)
#' plotvar(fit, "area_total", interval = "confidence")
#' plotvar(fit, "area_total", "log", interval = "confidence")
#' plotvar(fit, "area_total", interval = "confidence", 
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2, dist_b_mar = 250, padrao = "medio"))

#' plotvar(fit, "padrao", interval = "confidence")
#' plotvar(fit, "padrao", "log", interval = "confidence")

plotvar <- function(object, variable, func,
                    interval = c("none", "confidence", "prediction"),
                    level = 0.80, local, ...){
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
    if (missing(local)) {
      new <- data.frame(grid, lapply(df[setdiff(preds, variable)], centre))
      p_local <- NULL
    } else {
      new <- data.frame(grid, local) 
      p_local <- predict(fit, newdata = local)
    }
    names(new)[1] <- variable
    Y <- stats::predict.lm(object = z, newdata = new, interval = interval,
                           level = level, ...)
    if (!missing(func)) Y <- inverse(Y, func)
    pred <- data.frame(grid, Y)
    colnames(pred)[1] <- variable
    pred_plot <- reshape2::melt(pred, id.var = variable, value.name = response)
    p <- ggplot(data = pred_plot, aes_(x = as.name(variable), y = as.name(response))) + 
      geom_boxplot(aes_(fill = as.name(variable))) +                
      theme(legend.position="bottom")
    if(!missing(local)) {
      p_local <- ifelse(missing(func), p_local, inverse(p_local, func))
      p_local <- data.frame(y = p_local, local)
      names(p_local)[1] <- response
      p <- p + geom_point(data = p_local, 
                          aes_(x = as.name(variable), 
                               y = as.name(response)),
                          color = "red", 
                          size = 2)
    }
  } else {
    grid <- seq(min(df[, variable], na.rm = TRUE), max(df[, variable], na.rm = TRUE),
                length = 101)
    if (missing(local)) {
      new <- data.frame(grid, lapply(df[setdiff(preds, variable)], centre))
      p_local <- NULL
    } else {
      new <- data.frame(grid, local)  
      p_local <- predict(fit, newdata = local)
    }
    names(new)[1] <- variable
    Y <- stats::predict.lm(object = z, newdata = new, interval = interval,
                           level = level, ...)
    if (!missing(func)) Y <- inverse(Y, func)
    pred <- data.frame(grid, Y)
    colnames(pred)[1] <- variable
    colnames(pred)[2] <- response
    p <- ggplot(data = pred, aes_(x = as.name(variable), y = as.name(response))) +
      geom_line() + theme(legend.position="bottom")
    if(!missing(local)) {
      p_local <- ifelse(missing(func), p_local, inverse(p_local, func))
      p_local <- data.frame(y = p_local, local)
      names(p_local)[1] <- response
      p <- p + geom_point(data = p_local, 
                          aes_(x = as.name(variable), 
                               y = as.name(response)),
                               color = "red", 
                               size = 2)
    }
    if (interval != "none") {
      p <- p + geom_ribbon(aes(ymin = lwr, ymax = upr, colour = "grey", alpha = 0.5),
                           stat = "identity") +                                           
        theme(legend.position="none")       
    }
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
#' best_fit <- bestfit(valor ~ .,  data = data)
#' plotmod(best_fit, interval = "confidence")
#' plotmod(best_fit, fit = 2, interval = "confidence")
#' plotmod(best_fit, interval = "confidence", 
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2, dist_b_mar = 250, padrao = "medio"))
#'
#' fit <- lm(log(valor) ~ ., data = data)
#' plotmod(fit, interval = "confidence")
#' plotmod(fit, interval = "confidence", func = "log")
#' plotmod(fit, interval = "confidence", 
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2, dist_b_mar = 250, padrao = "medio"))
#' plotmod(fit, interval = "confidence", func = "log",
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2, dist_b_mar = 250, padrao = "medio"))
#' 
plotmod <- function(object, ...) UseMethod("plotmod")

#' @rdname plotmod
#' @export

plotmod.bestfit <- function(object, fit = 1, 
                            interval = c("none", "confidence", "prediction"),
                            level = 0.80,
                            local, ...){
  s <- summary(object, fit = fit)
  z <- s$fit
  func <- as.character(s$bestfit[, object$response])
  terms <- object$predictors
  
  r <- length(terms)
  par1 <- round(sqrt(r))
  par2 <- ceiling((r)/par1)
  p <- list()

  for (i in terms) {
    if (missing(local)) {
      p[[i]] <- plotvar(object = z, variable = i, func = func, 
                        interval = interval)
    } else {
      p[[i]] <- plotvar(object = z, variable = i, func = func, 
                        interval = interval, local = local)
    }
  }

  est <- list(plots = p,
              par1 = par1,
              par2 = par2)
  class(est) <- "plotmod.bestfit"
  return(est)
}

#' @rdname plotmod
#' @export
plotmod.lm <- function(object, 
                       interval = c("none", "confidence", "prediction"), 
                       level = 0.80, 
                       func = "identity",
                       local, ...){
  z <- object
  preds <- parameters(z)$predictors
  
  r <- length(preds)
  par1 <- round(sqrt(r))
  par2 <- ceiling((r)/par1)
  p <- list()
  
  for (i in preds) {
    if (missing(local)) {
      p[[i]] <- plotvar(object = z, variable = i, func = func, 
                        interval = interval)
    } else {
      p[[i]] <- plotvar(object = z, variable = i, func = func, 
                        interval = interval, local = local)
    }
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