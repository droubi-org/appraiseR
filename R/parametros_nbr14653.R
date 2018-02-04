#'Based on a dataframe containing the estimated values, returns amplitude
#'according to NBR14.653-2
#'
#'Amplitude according to NBR14.653-2.
#'
#'@param Y dataframe with \emph{Y_hat} values with confidence/prediction
#'  \code{\link{bestfit}}
#'  
#'@return a vector containing the amplitudes of the \emph{Y_hat} in \%
#'  
#'@export
#'@examples
#'fit <- lm(log(Valor_Total) ~ . - lat - lon, data = centro_2015)
#'aval <- grepl("^aval", rownames(centro_2015))
#'Y_hat <- predict(object = fit, interval = "confidence", newdata = centro_2015[aval,])
#'Y_hat <- inverse(Y_hat, "log")
#'amplitude(Y_hat)
amplitude <- function(Y){
  if (is.null(dim(Y))) amp <- NULL else {
    amp <- apply(Y, 1, function (x) 100 * (max(x)-min(x))/x[1])
    amp <- round(amp,2)
  }
  return(amp)
}

#'Based on a dataframe containing the estimated values, gives precision degree
#'parameters based in NBR14.653-2
#'
#'Precision degree based on NBR14.653-2.
#'
#'@param amplitude a vector containing the amplitudes of the estimations
#'  \code{\link{bestfit}}
#'  
#'@return a vector containing the precision degrees of the estimations.
#'  
#'@export
#'@examples
#'fit <- lm(log(Valor_Total) ~ . - lat - lon, data = centro_2015)
#'aval <- grepl("^aval", rownames(centro_2015))
#'Y_hat <- predict(object = fit, interval = "confidence", newdata = centro_2015[aval,])
#'Y_hat <- inverse(Y_hat, "log")
#'amp <- amplitude(Y_hat)
#'g_precisao(amp)
#'
g_precisao <- function(amplitude) {
  # Calcula a amplitude do intervalo
  amp <- amplitude
  
  # Calcula o grau de precisão baseado na amplitude do intervalo  
  if (is.null(amp)) gp <- NULL else {
    gp <- sapply(amp/100,  function (x) ifelse(x > .5, "Fora de Especifica\u00E7\u00E3o",
                                         ifelse(x <= .3, "III",
                                                ifelse(x <= .4, "II", "I"))))
  }
  return(gp)
}

#'Based on a dataframe containing the estimated values, returns interval of 
#'arbitration parameters according to NBR14.653-2
#'
#'Interval of arbitration according to NBR14.653-2.
#'
#'@param Y dataframe with \emph{Y_hat} values with or without
#'  confidence/prediction \code{\link{bestfit}}
#'  
#'@return a vector containing the interval of arbitration according to
#'  NBR14.653-2.
#'  
#'@export
#'@examples
#'fit <- lm(log(Valor_Total) ~ . - lat - lon, data = centro_2015)
#'aval <- grepl("^aval", rownames(centro_2015))
#'Y_hat <- predict(object = fit, interval = "confidence",
#'                 newdata = centro_2015[aval,])
#'Y_hat <- inverse(Y_hat, "log")
#'campo_arbitrio(Y_hat)
#' 
campo_arbitrio <- function(Y){
  ca <- matrix(data = c(0.85*Y[,"fit"], 1.15*Y[,"fit"]), nrow = nrow(Y), 
               dimnames = list(rownames(Y), c("C.A.I.", "C.A.S.")))
  return(ca)
}

#'Based on a dataframe containing the estimated values, returns interval evaluation
#'according to NBR14.653-2
#'
#'Interval evaluation based on NBR14.653-2.
#'
#'@param Y dataframe with \emph{Y_hat} values with or without confidence/prediction 
#'\code{\link{bestfit}}
#'
#'@return a vector containing interval evaluation according to NBR14.653-2.
#'
#'@export
#'@examples
#'fit <- lm(log(Valor_Total) ~ . - lat - lon, data = centro_2015)
#'aval <- grepl("^aval", rownames(centro_2015))
#'Y_hat <- predict(object = fit, interval = "confidence",
#'                 newdata = centro_2015[aval,])
#'Y_hat <- inverse(Y_hat, "log")
#'aval_intervalar(Y_hat)
#' 
aval_intervalar <- function(Y){
  if (is.null(dim(Y))) avi <- NULL else {
    ca <- campo_arbitrio(Y)
    avi <- matrix(c(pmax(ca[,"C.A.I."], apply(Y, 1, min)), 
                    pmin(ca[,"C.A.S."], apply(Y, 1, max))),
                  nrow = nrow(Y),
                  dimnames = list(rownames(Y), c("L.I.", "L.S.")))
  }
  return(avi)
}