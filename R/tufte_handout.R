#' Tufte handout format (PDF)
#'
#' This function has been moved to the \pkg{tufte} package. Please use
#' \code{tufte::tufte_handout} instead. See the
#' \href{https://bookdown.org/yihui/rmarkdown/tufte-handouts.html}{online
#' documentation} for additional details.
#' @inheritParams pdf_document
#' @export
tufte_handout <- function(fig_width = 4,
                          fig_height = 2.5,
                          fig_crop = TRUE,
                          dev = 'pdf',
                          highlight = "default",
                          keep_tex = FALSE,
                          citation_package = c("default", "natbib", "biblatex"),
                          includes = NULL,
                          md_extensions = NULL,
                          pandoc_args = NULL) {

  # TODO: remove tufte_handout from rmarkdown
  warning(
    'The function rmarkdown::tufte_handout() has been deprecated. Please use',
    'tufte::tufte_handout instead.'
  )

  # Confirm we have the tufte package available
  if (!requireNamespace("tufte", quietly = TRUE))
    stop("The 'tufte' package is required to render the tufte_handout format.")

  # Delegate to the tufte package
  tufte::tufte_handout(
    fig_width = fig_width,
    fig_height = fig_height,
    fig_crop = fig_crop,
    dev = dev,
    highlight = highlight,
    keep_tex = keep_tex,
    citation_package = citation_package,
    includes = includes,
    md_extensions = md_extensions,
    pandoc_args = pandoc_args)
}
