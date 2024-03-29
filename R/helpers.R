#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom magrittr %$%
#' @export
magrittr::`%$%`

#' @importFrom stats pnorm
#' @export
stats::pnorm

#' @importFrom stats predict
#' @export
stats::predict

#' Parent Function: Power of a number.
#'
#' This is an internal function to generate another ones which will be
#' effectivelly exported. This function is not exported in NAMESPACE, therefore
#' it is not available for the end-user.
#'
#' @param exponent The exponent.
#' @return A parent function that allows the user to create a closure that
#'   returns the n-th power of its argument.
#' @examples
#'
#' \dontrun{
#' power <- function(exponent) {
#' function(x) {
#'   x ^ exponent
#' }
#' }
#'
#' square <- power(2)
#' square_root <- power(.5)
#'
#' square(2) #4
#' square_root(4) #2
#' }

power <- function(exponent) {
    function(x) {
        x^exponent
    }
}

#' Reciprocal of the square of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' rsqr(2)
#' rsqr(1:10)

rsqr <- power(-2)

#' Reciprocal (1/x) of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' rec(2)
#' rec(1:10)

rec <- power(-1)

#' Reciprocal of the square root of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' rsqrt(4)
#' rsqrt(1:10)

rsqrt <- power(-0.5)

#' Square of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' sqr(2)
#' sqr(1:10)

sqr <- power(2)


#' Returns a vector generated with the inverse of the function f
#'
#' @param x A vector or object of type
#' @param func a function of the box-cox family (rsqr(), rec(), rsqrt(), log(),
#'   sqrt(), I() and sqr())
#' @return a
#' @export
#' @examples
#' fit <- lm(log(valor) ~ ., data = centro_2015@data)
#' aval <- new_data(fit)
#' Y <- predict(fit, newdata = aval, interval = "confidence")
#' inverse(Y, "log")

inverse <- function(x, func) {
    switch(func,
           rsqr = appraiseR::rsqrt(x),
           rec = appraiseR::rec(x),
           rsqrt = appraiseR::rsqr(x),
           log = exp(x),
           sqrt = appraiseR::sqr(x),
           identity = identity(x),
           sqr = sqrt(x)
           )
}


#' Return the median value or the modal value of a vector, depending on whether
#' the vector is numeric or a factor.
#'
#' @inheritParams stats::median
#' @return the median value for objects of the class integer or double. The
#'   modal value for objects of class factor.
#' @name centre
#' @export
centre <- function(x, ...) UseMethod("centre")

#' @rdname centre
#' @examples
#' vec <- c(-3, -2, 0, 1, 1, 3)
#' centre(vec)
#' @export
centre.numeric <- function(x, na.rm = TRUE, ...) {
  x <- stats::median(x, na.rm = na.rm, ...)
  x
}

#' @rdname centre
#' @examples
#' vec <- c(-3, -2, 0, 1, 1, 3)
#' vec <- as.factor(vec)
#' centre(vec)
#' centre(itacorubi_2015@data$padrao)
#' @export
centre.factor <- function(x, na.rm = TRUE, ...){
  x <- raster::modal(x, na.rm = na.rm)
  x
}

#' Extract object parameters
#'
#' Returns the parameters used to build a model.
#'
#' @param object A model object.
#' @param \dots not used.
#' @return the parameters, predictors and response names besides the
#' original data used to build the model.
#' @name parameters
#' @export
parameters <- function(object, ...) {
    UseMethod("parameters")
}


#' @rdname parameters
#' @examples
#' fit <- lm(log(valor) ~ ., centro_2015@data)
#' p <- parameters(fit)
#' p$parameters
#' p$predictors
#' p$response
#' p$data
#' @export
#'
parameters.lm <- function(object, ...) {
    z <- object
    cl <- stats::getCall(z)
    data <- eval(cl$data)
    vars <- all.vars(stats::formula(z))
    termsLabels <- attr(stats::terms.formula(stats::formula(z),
                                             data = data
                                             ),
                        "term.labels"
                        )
    x <- sapply(vars, grepl, termsLabels)
    preds <- vars[apply(x, 2, any)]
    resp <- vars[!apply(x, 2, any)]

    param <-
      list(parameters = c(resp, preds),
           predictors = preds,
           response = resp,
           data = data,
           call = cl)

    return(param)
}

#' @rdname parameters
#' @examples
#' best_fit <- bestfit(valor ~ ., centro_2015@data)
#' parameters(best_fit)
#' @export
#'
parameters.bestfit <- function(object, ...) {
    z <- object
    cl <- z$call
    data <- eval(cl$data, environment(stats::formula(z)))

    resp <- z$response
    preds <- z$predictors

    param <-
      list(parameters = c(resp, preds),
           predictors = preds,
           response = resp,
           data = data,
           call = cl)

    return(param)
}

#' Builds \code{newdata} argument to be used in \link{predict.lm}
#'
#' Builds a new \code{data.frame} containing only elements
#' to be appraised from the current \code{data.frame}
#'
#' @param object object of class \code{lm}
#'
#' @examples
#' fit <- lm(log(valor) ~ ., data = centro_2015@data)
#' new_data(fit)
#' @export

new_data <- function(object) {
  z <- object
  params <- parameters(z)
  response <- params$response
  response <- as.name(response)
  parameters <- params$parameters
  data <- params$data
  aval <-
    data %>%
    dplyr::filter(is.na(!!response)) %>%
    dplyr::select(parameters)
  aval
}
#' Wrapper around format
#' 
#' This function is only a wrapper around \link{format} function that uses standard 
#' brazillian formats by default
#' 
#' @param x a number to be formatted by \code{format}
#' @export
brformat <- function(x, decimal.mark = ",", big.mark = ".", digits = 2, 
                     nsmall = 2, scientific = FALSE, ...) {
  format(x, decimal.mark = decimal.mark, big.mark = big.mark, digits = digits, 
         nsmall = nsmall, scientific = scientific, ...)
}
#' Wrapper around brformat
#' 
#' This is a wrapper around \link{brformat}.
#' 
#' @param prefix currency units. Defaults for brazilian reais.
#' @param \ldots further arguments to be passed to \link{brformat}.
#' 
#' @export
reais <- function(prefix = "R$", ...) {
  function(x) paste(prefix, brformat(x, ...), sep = "")
}