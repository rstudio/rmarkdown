#' Run a Shiny document
#'
#' Start a Shiny server for the given document, and render it for display.
#'
#' @param file Input file
#' @param auto_reload If \code{TRUE} (the default), automatically reload the
#'   Shiny application when the input file is changed.
#' @param shiny_args Arguments to the \code{\link{runApp}} function.
#' @param ... Additional arguments to \code{\link{render}}.
#'
#' @return Invisible NULL.
#'
#' @details The \code{run_document} function runs a Shiny document by starting a
#'   \pkg{shiny} server associated with the document, and rendering the document
#'   as necessary when connections to the server are made.
#'
#' @note Unlike \code{\link{render}}, \code{run_document} does not render the
#'   document to a file on disk. To view the document, point a Web browser to
#'   the URL displayed when the server starts. In most cases a Web browser will
#'   be started automatically to view the document; see \code{launch.browser} in
#'   the \code{\link{runApp}} documentation for details.
#'
#' @examples
#' \dontrun{
#'
#' # Run the Shiny document "shiny_doc.Rmd" on port 8241
#'
#' rmarkdown::run_document("shiny_doc.Rmd", shiny_args = list(port = 8241))
#'
#' }
#' @export
run_document <- function(file, auto_reload = TRUE, shiny_args = NULL, ...) {

  server <- function(input, output, session) {
    reactive_file <- if (auto_reload)
      shiny::reactiveFileReader(500, session, file, identity)
    else
      function () { file }
    doc <- reactive({
      output_dest <- tempfile()
      on.exit(unlink(output_dest), add = TRUE)
      output_dest <- render(reactive_file(), output_file = output_dest,
                            runtime = "shiny", ...)
      paste(readLines(output_dest), collapse="\n")
    })
    output$`__reactivedoc__` <- renderUI({
      HTML(doc())
    })
  }
  args <- c(list(list(ui = shiny::fluidPage(shiny::uiOutput("__reactivedoc__")),
                 server = server)),
            shiny_args)
  do.call(shiny::runApp, args)
}

