#' Generates map from SpatialPointsDataFrame
#'
#' @param data A SpatialPointsDataFrame.
#' @param proj projection definition
#' @return A ggmap plot
#' @export
#' @examples
#'
#' gen_map(centro_2015)

gen_map <- function(data, proj = "+proj=utm +zone=22 +south +ellps=WGS84 +units=m +no_defs"){
  coords <- proj4::project(as.matrix(sp::coordinates(data)), 
                           proj = proj, 
                           inverse = TRUE)
  
  m <- ggmap::get_map(location = apply(coords, 2, mean), zoom = 15)
  df <- as.data.frame(coords)
  colnames(df) <- c("lon", "lat")
  ggmap::ggmap(m) + 
    geom_point(data = df, aes(x = lon, y = lat, color = "red")) +
    guides(color = FALSE) 
}