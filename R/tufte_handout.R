#' Tufte handout format (PDF)
#'
#' Template for creating a handout according to the style of
#' Edward R. Tufte and Richard Feynman.
#'
#' See the \href{https://rmarkdown.rstudio.com/tufte_handout_format.html}{online
#' documentation} for additional details.
#'
#' Creating Tufte handout output from R Markdown requires that LaTeX be installed.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations in the
#' \href{https://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html}{Bibliographies
#' and Citations} article in the online documentation.
#' @inheritParams pdf_document
#' @export
tufte_handout <- function(fig_width = 4,
                          fig_height = 2.5,
                          fig_crop = TRUE,
                          dev = 'pdf',
                          highlight = "default",
                          keep_tex = FALSE,
                          citation_package = c("none", "natbib", "biblatex"),
                          includes = NULL,
                          md_extensions = NULL,
                          pandoc_args = NULL) {

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
