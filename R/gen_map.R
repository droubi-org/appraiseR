#' Generates map from SpatialPointsDataFrame
#'
#' @param data A SpatialPointsDataFrame.
#' @param proj projection definition
#' @return A ggmap plot
#' @export
#' @examples
#'
#' gen_map(centro_2015)

gen_map <- function(data, 
                    proj = "+proj=utm +zone=22 +south +ellps=WGS84 +units=m +no_defs",
                    api_key){
  coords <- proj4::project(as.matrix(sp::coordinates(data)), 
                           proj = proj, 
                           inverse = TRUE)
  colnames(coords) <- c("lon", "lat")
  coords <- as.data.frame(coords)
  bb <- c(left = min(coords[, "lon"]), bottom = min(coords[, "lat"]), 
          right = max(coords[, "lon"]), top = max(coords[, "lat"]))
  ggmap::register_google(key = api_key)
  m <- ggmap::get_map(bbox = bb)

  ggmap::ggmap(m) + 
    geom_point(data = coords, aes(x = lon, y = lat, color = "red")) +
    guides(color = FALSE) 
}
