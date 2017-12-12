#' Run gene.alignment.table examples
#'
#' Launch a \code{gene.alignment.tables} example Shiny app using the built-in
#' datasets. 
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runExample()
#' }
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "hbv-alignment-viz", package = "gene.alignment.tables")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `gene.alignment.tables`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}