
#' Create HTML output
#'
#' Create HTML output for display within an R Markdown document, the RStudio
#' Viewer pane, or an external web browser.
#'
#' @param html HTML output
#' @param dependencies List of HTML output dependencies (creating using
#'   \code{\link{html_dependency}})
#' @param x Object to print as HTML
#'
#' @return Object of class \code{html_output} that when printed will render in a
#'   browser (either in RStudio or extenally) and when printed from within an R
#'   Markdown document will render inline.
#'
#' @details See the documentation on
#'   \href{http://rmarkdown.rstudio.com/developer_custom_html_output.html}{R
#'   Markdown Custom HTML Output} for examples and additional details.
#'
#' @export
html_output <- function(html, dependencies) {
  structure(class = "html_output", list(
    html = html,
    dependencies = dependencies
  ))
}

#' Define an HTML dependency
#'
#' Define an HTML dependency (e.g. CSS or Javascript and related library). HTML
#' dependency definitions are required for \code{\link{html_output}} that
#' require CSS or JavaScript within the document head to render correctly.
#'
#' @param name Library name
#' @param version Library version
#' @param path Full path to library
#' @param meta Named list of meta tags to insert into document head
#' @param script Script(s) to include within the document head (should be
#'   specified relative to the \code{path} parameter).
#' @param stylesheet Stylesheet(s) to include within the document (should be
#'   specified relative to the \code{path} parameter).
#' @param head Arbitrary lines of HTML to insert into the document head
#'
#' @return An object that can be included in the list of dependencies passed to
#'   \code{\link{html_output}}.
#'
#' @details See the documentation on
#'   \href{http://rmarkdown.rstudio.com/developer_custom_html_output.html}{R
#'   Markdown Custom HTML Output} for examples and additional details.
#'
#' @export
html_dependency <- function(name,
                            version,
                            path,
                            meta = NULL,
                            script = NULL,
                            stylesheet = NULL,
                            head = NULL) {
  structure(class = "html_dependency", list(
    name = name,
    version = version,
    path = path,
    meta = meta,
    script = script,
    stylesheet = stylesheet,
    head = head
  ))
}

#' @export
print.html_output <- function(x, ...) {

  # define temporary directory for output
  www_dir <- tempfile("viewhtml")
  dir.create(www_dir)

  # we want to re-use the html_dependencies_as_string function and also
  # ensure that the paths to dependencies are relative to the base
  # directory where the temp index.html is being built. to affect
  # this we setwd to the www_dir for the duration of this function
  # to a relative path
  oldwd <- setwd(www_dir)
  on.exit(setwd(oldwd), add = TRUE)

  # build the web-page
  html <- c("<!DOCTYPE html>",
            "<html>",
            "<head>",
            "<meta charset=\"utf-8\"/>",
            html_dependencies_as_string(x$dependencies, "lib"),
            "</head>",
            "<body>",
            x$html,
            "</body>",
            "</html>")

  # write it
  index_html <- file.path(www_dir, "index.html")
  writeLines(html, index_html, useBytes = TRUE)

  # show it
  viewer <- getOption("viewer")
  if (!is.null(viewer))
    viewer(index_html)
  else
    utils::browseURL(index_html)

  invisible(NULL)
}

#' @rdname html_output
#' @export
knit_print.html_output <- function(x) {
  structure(class = "knit_asis",
    x$html,
    knit_meta = x$dependencies
  )
}
