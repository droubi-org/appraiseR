# RETORNA O VALOR DAS  VARIÁVEIS EXTRAPOLADAS, O PERCENTUAL EXTRAPOLADO EM
# RELAÇÃO AOS VALORES MÁXIMOS E MÍNIMOS, A FRONTEIRA DE CADA AVALIANDO E O GRAU
# DE FUNDAMENTAÇÃO DE CADA AVALIAÇÃO SEGUNDO A NBR 14.653-2

#' Extrapolation limits
#' 
#' \code{extrapolate} evalues the extrapolation eventually used in the
#' appraisals and returns the precision degree as defined in brazilian standard
#' NBR-14.653-2 .
#' 
#' @param object object of type bestfit
#' @param newdata data frame with the real estate data to be appraised
#' @export
#' @examples
#' ## newdata inside the data file
#' fit <- lm(log(Valor_Total) ~ . - lat - lon, centro_2015)
#' extrapolate(object = fit)
#' 
#' ## newdata provided as an argument
#' rows <- grepl("^aval", rownames(centro_2015))
#' cols <- attr(terms.formula(formula(fit)), "term.labels")
#' newdata <- subset(centro_2015, subset = rows, select = cols)
#' newdata$Area_Total[1] <- 1.5*max(centro_2015$Area_Total[1:50])
#' newdata$Dist_Beira_Mar[2] <- .5*min(centro_2015$Dist_Beira_Mar[1:50])
#' 
#' extrapolate(object = fit, newdata = newdata)

extrapolate <- function(object, newdata){

  z <- object
  cl <- stats::getCall(z)
  data <- eval(cl$data, environment(stats::formula(z)))
  param <- parameters(z)
  preds <- param$predictors
  
  if (missing(newdata))
    newdata <- base::subset(data, subset = grepl("^aval", rownames(data)),
                      select = preds)
  id <- NULL
  id2 <- NULL
  maxs <- plyr::numcolwise(max)(data[preds])
  mins <- plyr::numcolwise(min)(data[preds])
  numeric <- plyr::colwise(is.numeric)(newdata)
  num_preds <- which(numeric == TRUE)
  newdata_num <- newdata[num_preds]
  extr <- plyr::adply(newdata_num, 1,
                      function(x) x - pmin(x, maxs) + x - pmax(x, mins))
  rownames(extr) <- rownames(newdata)

  ## Verifies if there is extrapolation
  x <- matrix(data = 0, nrow = nrow(extr), ncol = ncol(extr))
  x <- as.data.frame(x, row.names = row.names(extr))
  colnames(x) <- colnames(extr)

  if (isTRUE(all.equal(x, extr))) {
    extr_p <- NULL
    front <- NULL
    y <- NULL
    x <- NULL
  } else {
    ## Computes the extrapoled percentual
    extr <- as.matrix(extr)
    extrPPlus <- plyr::adply(extr, 1,
                            function(x)  x[x>0]/maxs[names(x[x>0])], .id = "id")
    extrPMinus <- plyr::adply(extr, 1,
                            function(x)  x[x<0]/mins[names(x[x<0])], .id = "id")
    extr_p <- merge(extrPPlus, extrPMinus, by="id", all.x = TRUE, all.y=TRUE)
    rownames(extr_p) <- extr_p[,"id"]
    extr_p <- subset(extr_p, select = -id)

    ## Computes the frontier of the variables for each newdata row
    front <- plyr::adply(newdata, 1, function(x) pmax(pmin(x, maxs), mins))
    rownames(front) <- rownames(newdata)

    ## Classifie the degrees of the extrapolation for each predictor
    y <- plyr::adply(extr_p, c(1,2),
                     function(x) ifelse(x > 1 | x < -.5,
                                        sprintf("OUT", 100*x),
                                        sprintf("%.2f%%(Grau I/II)", 100*x)),
                     .id = c("id1","id2"))

    y <- subset(y, subset = rowSums(is.na(y)) != ncol(y)-2, select = -id2)

    ## Classifie each appraisal for extrapolation
    nvar_extrap <- as.data.frame(rowSums(!is.na(extr_p)))
    colnames(nvar_extrap) <- 'N_var'
    x <- apply(nvar_extrap, 1, function(x) if (x > 1) "I" else "II")
  }
  return(list(valor = extr,
              percentual = extr_p,
              fronteira = front,
              grau_variaveis = y,
              grau_aval = x))
}
