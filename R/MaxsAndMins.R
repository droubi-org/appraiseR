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
#' 
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
    select(colnames(newdata)) %>%
    select_if(is.numeric) %>% 
    summarize_all(max, na.rm = TRUE) %>%
    unlist()
  mins <- 
    data %>% 
    select(colnames(newdata)) %>%
    select_if(is.numeric) %>% 
    summarize_all(min, na.rm = TRUE) %>%
    unlist()
  df <- list()
  for (i in seq_len(dim(newdata)[1])){
    x <- 
      newdata %>% 
      select_if(is.numeric) %>%
      slice(!!i) %>% 
      unlist()
    df[[i]] <- x - pmin(x, maxs) + x - pmax(x, mins)  
  }
  return(rbind_list(df))
}
