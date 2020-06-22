notebook_render_html_widget <- function(output) {

  unpreserved <- htmltools::extractPreserveChunks(output)

  meta <- base64_encode_object(attr(output, "knit_meta"))

  before <- sprintf("\n<!-- rnb-htmlwidget-begin %s-->", meta)
  after  <- "<!-- rnb-htmlwidget-end -->\n"
  pasted <- paste(before, unpreserved$value, after, sep = "\n")

  annotated <- htmltools::restorePreserveChunks(pasted,
                                                unpreserved$chunks)
  attributes(annotated) <- attributes(output)

  return(annotated)
}
