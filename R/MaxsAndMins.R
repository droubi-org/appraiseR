#' MaxsAndMins
#' 
#' Find out how much new data extrapolates the model data
#'
#' @param newdata a tibble with newdata1
#' @param maxs a vec
#' @param mins 
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' data(centro_2015)
#' data <- centro_2015@data#' 
#' newdata <- 
#'   centro_2015@data %>% 
#'   filter(is.na(valor)) %>%
#'   select(-valor)
#' MaxsAndMins(newdata, data)   
#'
#'   
MaxsAndMins <- function(newdata, data) {
  maxs <- 
    data %>% 
    dplyr::select(colnames(newdata)) %>%
    dplyr::select_if(is.numeric) %>% 
    dplyr::summarize_all(max, na.rm = TRUE) %>%
    unlist()
  mins <- 
    data %>% 
    dplyr::select(colnames(newdata)) %>%
    dplyr::select_if(is.numeric) %>% 
    dplyr::summarize_all(min, na.rm = TRUE) %>%
    unlist()
  df <- list()
  for (i in seq_len(dim(newdata)[1])){
    x <- 
      newdata %>% 
      dplyr::select_if(is.numeric) %>%
      dplyr::slice(!!i) %>% 
      unlist()
    df[[i]] <- x - pmin(x, maxs) + x - pmax(x, mins)  
  }
  return(rbind_list(df))
}
