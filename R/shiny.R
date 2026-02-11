#' Interactive aggregate explorer
#'
#' Launches a Shiny app for browsing and filtering the full catalog of IBGE
#' aggregates. The app displays value boxes with summary counts, a searchable
#' table with column-level filters, and lets you filter by survey or search
#' aggregate names.
#'
#' Clicking a row shows a notification with the aggregate ID and the
#' corresponding `ibge_metadata()` call. A CSV download button is also
#' available.
#'
#' @param launch.browser Logical or function. If `TRUE`, opens the app in
#'   the default browser. If `FALSE`, opens in the RStudio Viewer pane
#'   (default). You can also pass a function such as [shiny::paneViewer()].
#'
#' @return This function is called for its side effect (launching the app).
#'   Returns the value of [shiny::runApp()] invisibly.
#'
#' @examples
#' \dontrun{
#' # Open in RStudio Viewer
#' ibge_explorer(launch.browser = FALSE)
#'
#' # Open in browser
#' ibge_explorer()
#' }
#'
#' @export
ibge_explorer <- function(launch.browser = TRUE) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg shiny} package is required to run the aggregate explorer.",
      "i" = 'Install it with {.code install.packages("shiny")}'
    ), call = NULL)
  }
  
  if (!requireNamespace("DT", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg DT} package is required to run the aggregate explorer.",
      "i" = 'Install it with {.code install.packages("DT")}'
    ), call = NULL)
  }
  
  if (!requireNamespace("bslib", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg bslib} package is required to run the aggregate explorer.",
      "i" = 'Install it with {.code install.packages("bslib")}'
    ), call = NULL)
  }
  
  if (!requireNamespace("bsicons", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg bsicons} package is required to run the aggregate explorer.",
      "i" = 'Install it with {.code install.packages("bslib")}'
    ), call = NULL)
  }
  
  app_dir <- system.file("shiny", "explorer", package = "ibger")
  
  if (app_dir == "") {
    cli::cli_abort(
      "Could not find the explorer app. Try reinstalling {.pkg ibger}.",
      call = NULL
    )
  }
  
  if (isFALSE(launch.browser) &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    launch.browser <- shiny::paneViewer(minHeight = 600)
  }
  
  shiny::runApp(app_dir, launch.browser = launch.browser, display.mode = "normal")
}