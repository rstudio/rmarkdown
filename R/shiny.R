#' Run a Shiny document
#'
#' Render a Shiny document and start Shiny to show the output.
#'
#' @param input Input file
#' @param shiny_args Arguments to the \pkg{shiny} \code{\link{runApp}} function.
#' @param ... Additional arguments to \code{\link{render}}.
#'
#' @return Invisible NULL.
#'
#' @details The \code{run_document} function fully renders an R Markdown
#'   document to HTML (using \code{\link{render}}), creates a Shiny application
#'   from the result, and then invokes \code{runApp} to view the application.
#'
#'   Note that unlike \code{render}, \code{run_document} does not produce a
#'   file on disk that can be saved or shared.
#'
#' @examples
#' \dontrun{
#'
#' # Render shiny_doc.Rmd and start a server for the result on port 8241.
#' rmarkdown::run_document("shiny_doc.Rmd", shiny_args = list(port = 8241))
#'
#' }
#' @export
run_document <- function(input, shiny_args = NULL, ...) {
  # create a temporary folder to host the Shiny application
  appdir <- tempfile()
  dir.create(appdir)
  on.exit(unlink(appdir, recursive = TRUE), add = TRUE)
  wwwdir <- file.path(appdir, "www")
  dir.create(wwwdir)

  # load Shiny; needed to expose the Shiny knit output functions to widgets that
  # use them
  library(shiny)

  # render the document
  output <- file.path(wwwdir, "index.html")
  render(input, output_file = output, ...)
  if (!file.exists(output)) {
    stop("R Markdown rendering failed; no Shiny application produced")
  }

  # write a Shiny server stub
  writeLines("shinyServer(function(input, output) NULL)",
             file.path(appdir, "server.R"))

  # run the application
  args <- c(list(appdir), shiny_args)
  do.call(shiny::runApp, args)

  invisible(NULL)
}
