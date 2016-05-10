#' Generate R Notebook Output
#'
#' Utilities for generating output for the \code{html_notebook} format,
#' through the \code{output_source} function attached to a
#' \code{\link{output_format}}.
#'
#' @param path  A path to a file. For functions accepting both \code{path}
#'   and \code{bytes}, if \code{bytes} is \code{NULL}, the bytewise contents
#'   will be obtained by reading the file.
#' @param bytes The bytewise representation of content.
#' @param html Arbitrary HTML content to insert.
#' @param iframe Boolean; should the generated HTML be shown in an iframe?
#'
#' @name html_notebook_output
NULL

html_notebook_render_base64_data <- function(path = NULL,
                                             bytes = NULL,
                                             format)
{
  if (is.null(bytes))
    bytes <- read_file(path, binary = TRUE)
  encoded <- base64enc::base64encode(bytes)
  sprintf(format, encoded)
}

#' @name html_notebook_output
#' @export
html_notebook_output_html <- function(html, iframe = TRUE) {
  meta <- list(iframe = isTRUE(iframe))
  html_notebook_annotated_output(html, "html", meta)
}

#' @name html_notebook_output
#' @export
html_notebook_output_png <- function(path = NULL, bytes = NULL) {
  format <- '<img src="data:image/png;base64,%s" />'
  html <- html_notebook_render_base64_data(path, bytes, format)
  html_notebook_annotated_output(html, "plot")
}

