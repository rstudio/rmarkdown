#' Convert to an HTML fragment.
#'
#' An html fragment is suitable for inclusion into an external html paage.
#' See \code{\link{html_document}} for full details - this is a minor variation
#' that assumes you will include the output into an existing document (e.g.
#' a blog post).
#'
#' @inheritParams html_document
#' @return R Markdown output format to pass to \code{\link{render}}
#' @export
html_fragment <- function(number_sections = FALSE,
                          fig_width = 7,
                          fig_height = 5,
                          fig_retina = if (!fig_caption) 2,
                          fig_caption = FALSE,
                          dev = 'png',
                          smart = TRUE,
                          keep_md = FALSE,
                          pandoc_args = NULL,
                          ...) {

  html_document(
    number_sections = number_sections, fig_width = fig_width,
    fig_height = fig_height, fig_retina = fig_retina, fig_caption = fig_caption,
    dev = dev, smart = smart, keep_md = keep_md, pandoc_args = pandoc_args,
    mathjax = NULL, highlight = NULL, theme = NULL, ...,
    template = rmarkdown_system_file("rmd/fragment/default.html")
  )
}
