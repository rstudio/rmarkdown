#' Run a Shiny document
#'
#' Start a Shiny server for the given document, and render it for display.
#'
#' @param filename Path to the input R Markdown document, relative to
#'   \code{dir}.
#' @param dir The directory from which to to read input documents.
#' @param auto_reload If \code{TRUE} (the default), automatically reload the
#'   Shiny application when the input file is changed.
#' @param shiny_args Additional arguments to \code{\link[shiny:runApp]{runApp}}.
#' @param render_args Additional arguments to \code{\link{render}}.
#'
#' @return Invisible NULL.
#'
#' @details The \code{run} function runs a Shiny document by starting a Shiny
#'   server associated with the document. The \code{shiny_args} parameter can be
#'   used to configure the server; see the \code{\link[shiny:runApp]{runApp}}
#'   documentation for details.
#'
#'   Once the server is started, the document will be rendered using
#'   \code{\link{render}}. The server will initiate a render of the document
#'   whenever necessary, so it is not necessary to call \code{run} every time
#'   the document changes: if \code{auto_reload} is \code{TRUE}, saving the
#'   document will trigger a render. You can also manually trigger a render by
#'   reloading the document in a Web browser.
#'
#'   The server will render any R Markdown (\code{.Rmd}) document in \code{dir};
#'   the \code{filename} argument specifies only the initial document to be
#'   rendered and viewed. You can therefore link to other documents in the
#'   directory using standard Markdown syntax, e.g.
#'   \code{[Analysis Page 2](page2.Rmd)}.
#'
#' @note Unlike \code{\link{render}}, \code{run} does not render the document to
#'   a file on disk. To view the document, point a Web browser to the URL
#'   displayed when the server starts. In most cases a Web browser will be
#'   started automatically to view the document; see \code{launch.browser} in
#'   the \code{\link[shiny:runApp]{runApp}} documentation for details.
#'
#' @examples
#' \dontrun{
#'
#' # Run the Shiny document "shiny_doc.Rmd" on port 8241
#'
#' rmarkdown::run("shiny_doc.Rmd", shiny_args = list(port = 8241))
#'
#' }
#' @export
run <- function(filename, dir = getwd(), auto_reload = TRUE, shiny_args = NULL,
                render_args = NULL) {

  file <- file.path(dir, filename)
  if (!file.exists(file))
    stop(file, " does not exist")

  # create the Shiny server function
  server <- function(input, output, session) {
    path_info <- session$request$PATH_INFO
    path_info <- substr(path_info, 1, nchar(path_info) - 11)
    if (!nzchar(path_info)) {
      path_info <- "/index.Rmd"
    }

    file <- file.path(dir, path_info)
    reactive_file <- if (auto_reload)
      shiny::reactiveFileReader(500, session, file, identity)
    else
      function () { file }

    # when the file loads (or is changed), render to a temporary file, and
    # read the contents into a reactive value
    doc <- shiny::reactive({
      output_dest <- tempfile(fileext = ".html")
      resource_folder <- knitr_files_dir(output_dest)

      # ensure that the document is not rendered to one page
      output_opts <- list(
         self_contained = FALSE)

      # merge our inputs with those supplied by the user and invoke render
      args <- merge_lists(list(input = reactive_file(),
                               output_file = output_dest,
                               output_options = output_opts,
                               runtime = "shiny"),
                          render_args)
      result_path <- do.call(render, args)

      # if we generated a folder of supporting files, map requests to those
      # files in the Shiny application
      if (file.exists(resource_folder))
        addResourcePath(basename(resource_folder), resource_folder)

      # when the session ends, remove the rendered document and any supporting
      # files
      onReactiveDomainEnded(getDefaultReactiveDomain(), function() {
        unlink(result_path)
        unlink(resource_folder, recursive = TRUE)
      })
      paste(readLines(result_path, warn = FALSE), collapse="\n")
    })
    output$`__reactivedoc__` <- shiny::renderUI({
      shiny::HTML(doc())
    })
  }

  ui <- function(req) {
    # map requests to / to requests for index.Rmd
    req_path <- req$PATH_INFO
    if (identical(req_path, "/")) {
      req_path <- "/index.Rmd"
    }

    # request must be for an R Markdown document
    if (!identical(substr(req_path, nchar(req_path) - 3, nchar(req_path)),
                  ".Rmd")) {
      return(NULL)
    }

    # document must exist
    target_file <- file.path(dir, req$PATH_INFO)
    if (!file.exists(target_file)) {
      return(NULL)
    }

    shiny::uiOutput("__reactivedoc__")
  }

  # combine the user-supplied list of Shiny arguments with our own and start
  # the Shiny server
  app <- shiny::shinyApp(ui = ui,
                         uiPattern = "/.*.Rmd",
                         server = server)

  # launch the app and open a browser to the requested page
  launch_browser <- interactive()
  if (isTRUE(launch_browser)) {
    launch_browser <- function(url) {
      url <- paste(url, filename, sep = "/")
      browser <- getOption("shiny.launch.browser")
      if (is.function(browser)) {
        browser(url)
      } else {
        utils::browseURL(url)
      }
    }
  }

  shiny_args <- merge_lists(list(appDir = app,
                                 launch.browser = launch_browser),
                            shiny_args)
  do.call(shiny::runApp, shiny_args)
}

