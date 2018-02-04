#' Run shinyApp
#'
#' \code{runExample} starts a shinyApp that uses main functions in \code{REATOOLS} package
#' @export
#' @examples
#' \dontrun{
#' REATOOLS::runExample()
#'}
#'
runExample <- function() {
  appDir <- system.file("shiny-examples", "REApp", package = "REATOOLS")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `REATOOLS`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", port = 4949)
}
