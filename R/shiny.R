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
run_document <- function(file, shiny_args = NULL, ...) {

  shinyPlot <- create_adapter(renderPlot, plotOutput)
  shinyTable <- create_adapter(renderTable, tableOutput)
  shinyPrint <- create_adapter(renderPrint, verbatimTextOutput)

  server <- function(input, output, session) {
    output_dest <- tempfile()
    on.exit(unlink(output_dest), add = TRUE)
    output_dest <- render(file, output_file = output_dest, ...)
    doc <- readLines(output_dest)
    output$`__reactivedoc__` <- renderUI({
      HTML(doc)
    })
  }
  args <- c(list(list(ui = shiny::fluidPage(uiOutput("__reactivedoc__")),
                 server = server)),
            shiny_args)
  do.call(shiny::runApp, args)
}

create_adapter <- function(renderFunc, outputFunc) {
  function(expr, env = parent.frame(), quoted=FALSE) {
    installExprFunction(expr, "func", env, quoted)

    id <- shiny:::createUniqueId(8)
    o <- get("output", pos = env)
    o[[id]] <- renderFunc(func())
    outputFunc(id)
  }
}

