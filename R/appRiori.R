#' @importFrom hypr hypr
#' @importFrom sortable sortable_output
#' @importFrom stringr regex
#' @importFrom pracma gcd
#' @importFrom dplyr mutate
#' @importFrom stats formula
#' @importFrom shiny runApp
#' @importFrom shinythemes shinytheme
#' @importFrom DT renderDataTable
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom markdown html_format


#' @title Running appRiori
#'
#' @description This function runs the appRiori Shiny app.
#'
#'
#' @param ... Arguments passed on to \code{\link[shiny:runApp]{shiny::runApp()}}
#'
#' @examples
#'
#' \dontrun{
#' # Run the app with default settings
#' library(appRiori)
#' appRiori()
#' }
#'
#'
#' @seealso \code{\link[shiny:runApp]{runApp()}}
#'
#' @export
appRiori <- function(...) {
  appDir <- system.file("app", package = "appRiori")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `appRiori`.", call. = FALSE)
  }
  runApp(appDir = appDir, ...)
}
