#' Run a Shiny document
#'
#' Start a Shiny server for the given document, and render it for display.
#'
#' @param file Path to the R Markdown document to launch in a web browser.
#'   Defaults to \code{index.Rmd} in the current working directory, but may be
#'   \code{NULL} to skip launching a browser.
#' @param dir The directory from which to to read input documents. Defaults to
#'   the parent directory of \code{file}.
#' @param auto_reload If \code{TRUE} (the default), automatically reload the
#'   Shiny application when the file currently being viewed is changed on disk.
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
#'   the \code{file} argument specifies only the initial document to be
#'   rendered and viewed. You can therefore link to other documents in the
#'   directory using standard Markdown syntax, e.g.
#'   \code{[Analysis Page 2](page2.Rmd)}.
#'
#'   If you wish to share R code between your documents, place it in a file
#'   named \code{global.R} in \code{dir}; it will be sourced into the global
#'   environment.
#'
#' @note Unlike \code{\link{render}}, \code{run} does not render the document to
#'   a file on disk. In most cases a Web browser will be started automatically
#'   to view the document; see \code{launch.browser} in the
#'   \code{\link[shiny:runApp]{runApp}} documentation for details.
#'
#'   When using an external web browser with the server, specify the name of the
#'   R Markdown file to view in the URL (e.g.
#'   \code{http://127.0.0.1:1234/foo.Rmd}). A URL without a filename will show
#'   \code{index.Rmd}, if it exists in \code{dir}.
#'
#' @examples
#' \dontrun{
#'
#' # Run the Shiny document "index.Rmd" in the current directory
#' rmarkdown::run()
#'
#' # Run the Shiny document "shiny_doc.Rmd" on port 8241
#' rmarkdown::run("shiny_doc.Rmd", shiny_args = list(port = 8241))
#'
#' }
#' @export
run <- function(file = "index.Rmd", dir = dirname(file), auto_reload = TRUE,
                shiny_args = NULL, render_args = NULL) {

  # form and test locations
  dir <- normalizePath(dir)
  if (!file.exists(dir))
    stop("The directory '", dir, " does not exist")

  if (!is.null(file)) {
    file <- path.expand(file)

    # compute file path relative to directory (remove common directory prefix
    # if it exists)
    file_rel <- sub(paste("^", dir, "/", sep = ""), "", file)
    resolved <- resolve_relative(dir, file_rel)
    if (is.null(resolved) || !file.exists(resolved))
      stop("The file '", file, "' does not exist in the directory '", dir, "'")
  }

  # create the Shiny server function
  server <- function(input, output, session) {
    path_info <- utils::URLdecode(session$request$PATH_INFO)
    path_info <- substr(path_info, 1, nchar(path_info) - 11)
    if (!nzchar(path_info)) {
      path_info <- "index.Rmd"
    }

    file <- resolve_relative(dir, path_info)
    reactive_file <- if (auto_reload)
      shiny::reactiveFileReader(500, session, file, identity)
    else
      function () { file }

    # when the file loads (or is changed), render to a temporary file, and
    # read the contents into a reactive value
    doc <- shiny::reactive({
      output_dest <- tempfile(fileext = ".html")
      resource_folder <- knitr_files_dir(output_dest)

      # use a custom dependency resolver that just accumulates the dependencies
      # (we'll pass these to Shiny in a moment)
      dependencies <- list()
      shiny_dependency_resolver <- function(deps) {
        dependencies <<- deps
        list()
      }

      # ensure that the document is not rendered to one page
      output_opts <- list(
         self_contained = FALSE,
         copy_images = TRUE,
         dependency_resolver = shiny_dependency_resolver)

      # merge our inputs with those supplied by the user and invoke render
      args <- merge_lists(list(input = reactive_file(),
                               output_file = output_dest,
                               output_options = output_opts,
                               runtime = "shiny"),
                          render_args)
      result_path <- shiny::maskReactiveContext(do.call(render, args))

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
      structure(
        shiny::HTML(paste(readLines(result_path, encoding = "UTF-8", warn = FALSE),
                          collapse="\n")),
        html_dependency = dependencies)
    })
    output$`__reactivedoc__` <- shiny::renderUI({
      doc()
    })
  }

  ui <- function(req) {
    # map requests to / to requests for index.Rmd
    req_path <- utils::URLdecode(req$PATH_INFO)
    if (identical(req_path, "/")) {
      req_path <- "index.Rmd"
    }

    # request must be for an R Markdown document
    if (!identical(substr(req_path, nchar(req_path) - 3, nchar(req_path)),
                  ".Rmd")) {
      return(NULL)
    }

    # document must exist
    target_file <- resolve_relative(dir, req_path)
    if (is.null(target_file) || !file.exists(target_file)) {
      return(NULL)
    }

    shiny::uiOutput("__reactivedoc__")
  }

  onStart <- function() {
    global_r <- file.path.ci(dir, "global.R")
    if (file.exists(global_r)) {
      source(global_r, local = FALSE)
    }
  }

  # combine the user-supplied list of Shiny arguments with our own and start
  # the Shiny server; handle requests for the root (/) and any R markdown files
  # within
  app <- shiny::shinyApp(ui = ui,
                         uiPattern = "/|(/.*.Rmd)",
                         onStart = onStart,
                         server = server)

  # launch the app and open a browser to the requested page, if one was
  # specified
  launch_browser <- (!is.null(file)) && interactive()
  if (isTRUE(launch_browser)) {
    launch_browser <- function(url) {
      url <- paste(url, file_rel, sep = "/")
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
  invisible(NULL)
}

# resolve a path relative to a directory (from Shiny)
resolve_relative <- function(dir, relpath) {
  abs.path <- file.path(dir, relpath)
  if (!file.exists(abs.path))
    return(NULL)
  abs.path <- normalizePath(abs.path, winslash='/', mustWork=TRUE)
  dir <- normalizePath(dir, winslash='/', mustWork=TRUE)
  # trim the possible trailing slash under Windows
  if (.Platform$OS.type == 'windows') dir <- sub('/$', '', dir)
  if (nchar(abs.path) <= nchar(dir) + 1)
    return(NULL)
  if (substr(abs.path, 1, nchar(dir)) != dir ||
        substr(abs.path, nchar(dir)+1, nchar(dir)+1) != '/') {
    return(NULL)
  }
  return(abs.path)
}

# find 'name' in 'dir', without matching on case (from Shiny)
file.path.ci <- function(dir, name) {
  default <- file.path(dir, name)
  if (file.exists(default))
    return(default)
  if (!file.exists(dir))
    return(default)

  matches <- list.files(dir, name, ignore.case=TRUE, full.names=TRUE,
                        include.dirs=TRUE)
  if (length(matches) == 0)
    return(default)
  return(matches[[1]])
}

