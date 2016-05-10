#' Convert to an RTF document
#'
#' Format for converting from R Markdown to an RTF document.
#'
#' @inheritParams pdf_document
#' @inheritParams html_document
#' @inheritParams word_document
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' See the \href{http://rmarkdown.rstudio.com/rtf_document_format.html}{online
#' documentation} for additional details on using the \code{rtf_document} format.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations in the
#' \href{http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html}{Bibliographies
#' and Citations} article in the online documentation.
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' # simple invocation
#' render("input.Rmd", rtf_document())
#'
#' # specify table of contents option
#' render("input.Rmd", rtf_document(toc = TRUE))
#' }
#'
#' @export
rtf_document <- function(toc = FALSE,
                         toc_depth = 3,
                         fig_width = 5,
                         fig_height = 4,
                         keep_md = FALSE,
                         md_extensions = NULL,
                         pandoc_args = NULL) {

  # knitr options and hooks
  knitr <- knitr_options(
    opts_chunk = list(dev = 'png',
                      dpi = 96,
                      fig.width = fig_width,
                      fig.height = fig_height)
  )

  # build pandoc args
  args <- c("--standalone")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # pandoc args
  args <- c(args, pandoc_args)

  # return output format
  output_format(
    knitr = knitr,
    pandoc = pandoc_options(to = "rtf",
                            from = from_rmarkdown(extensions = md_extensions),
                            args = args),
    keep_md = keep_md
  )
}

