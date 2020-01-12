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
