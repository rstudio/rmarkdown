# Helpers for encoding content as HTML
# Consider moving some of these to htmltools?

notebook_render_html_widget <- function(output) {

  # TODO: add htmlUnpreserve function to htmlwidgets?
  unpreserved <- substring(
    output,
    n_bytes("<!--html_preserve-->") + 1,
    n_bytes(output) - n_bytes("<!--/html_preserve-->")
  )

  meta <- base64_encode_object(attr(output, "knit_meta"))

  before <- sprintf("\n<!-- rnb-htmlwidget-begin %s-->", meta)
  after  <- "<!-- rnb-htmlwidget-end -->\n"
  pasted <- paste(before, unpreserved, after, sep = "\n")

  annotated <- htmltools::htmlPreserve(pasted)
  attributes(annotated) <- attributes(output)

  return(annotated)
}

notebook_render_html <- function(path = NULL, bytes = NULL, format) {

  if (is.null(bytes))
    bytes <- read_file(path, binary = TRUE)

  encoded <- base64_encode_object(bytes)
  sprintf(format, encoded)
}

noteobok_render_png <- function(path = NULL, bytes = NULL) {
  format <- '<img src="data:image/png;base64,%s" />'
  notebook_render_html(path, bytes, format)
}

notebook_render_js <- function(path = NULL, bytes = NULL)
{
  format <- '<script src="data:application/x-javascript;base64,%s"></script>'
  notebook_render_html(path, bytes, format)
}

notebook_render_css <- function(path = NULL, bytes = NULL) {
  format <- '<link href="data:text/css;charset=utf8;base64,%s" />'
  notebook_render_html(path, bytes, format)
}

notebook_render_code <- function(code, attr_list = NULL) {
  attributes <- to_html_attributes(attr_list)
  if (nzchar(attributes)) attributes <- paste(" ", attributes, sep = "")
  pasted <- htmltools::htmlEscape(paste(code, collapse = "\n"))
  formatted <- sprintf("<pre%s><code>%s</code></pre>", attributes, pasted)
  knitr::asis_output(formatted)
}
