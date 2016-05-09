#' @rdname html_notebook_output
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

html_notebook_output_png <- function(path = NULL, bytes = NULL) {
  format <- '<img src="data:image/png;base64,%s" />'
  html <- html_notebook_render_base64_data(path, bytes, format)
  html_notebook_annotated_output(html, "plot")
}
