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
#' @param attributes A named \R list of HTML attributes. These will be
#'   escaped and inserted into the generated HTML as appropriate.
#' @param code Source code.
#' @param meta An \R list of arbitrary meta-data. The data will
#'   be converted to JSON, base64-encoded, and injected into the header comment.
#'
#' @details For more details on the HTML file format produced by
#'  \code{html_notebook}, see \href{http://rmarkdown.rstudio.com/r_notebook_format.html}{http://rmarkdown.rstudio.com/r_notebook_format.html}.
#'
#' @name html_notebook_output
NULL

#' Generate R Notebook Metadata
#'
#' A structured helper for the construction of metadata used by the
#' R Notebook output functions. See \code{\link{html_notebook_output}} for
#' more details.
#'
#' @param iframe Boolean; should output be shown in an \code{<iframe>}?
#' @export
html_notebook_metadata <- function(iframe = TRUE) {
  list(iframe = iframe)
}

html_notebook_render_base64_data <- function(path = NULL,
                                             bytes = NULL,
                                             attributes = NULL,
                                             format)
{
  # read (if necessary) and encode data
  if (is.null(bytes))
    bytes <- read_file(path, binary = TRUE)
  encoded <- base64enc::base64encode(bytes)

  # generate html attributes
  sprintf(format, to_html_attributes(attributes), encoded)
}

#' @name html_notebook_output
#' @export
html_notebook_output_html <- function(html,
                                      meta = NULL)
{
  html_notebook_annotated_output(paste(html, collapse = "\n"), "html", meta)
}

#' @name html_notebook_output
#' @export
html_notebook_output_img <- function(path = NULL,
                                     bytes = NULL,
                                     attributes = NULL,
                                     meta = NULL)
{
  format <- '<img%s src="data:image/png;base64,%s" />'
  html <- html_notebook_render_base64_data(path, bytes, attributes, format)
  html_notebook_annotated_output(html, "plot", meta)
}

#' @name html_notebook_output
#' @export
html_notebook_output_png <- html_notebook_output_img

#' @name html_notebook_output
#' @export
html_notebook_output_code <- function(code,
                                      attributes = list(class = "r"),
                                      meta = NULL)
{
  # normalize code
  joined <- htmltools::htmlEscape(join(code, collapse = "\n"))

  # generate html
  format <- '<pre%s><code>%s</code></pre>'
  html <- sprintf(format, to_html_attributes(attributes), joined)

  # update metadata
  if ("data" %in% names(meta)) {
    warning("'data' element of metadata will be overwritten")
    meta$data <- code
  }

  # render
  html_notebook_annotated_output(html, "source", meta)
}
