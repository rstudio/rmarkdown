#' Convert to an MS Word document
#'
#' Format for converting from R Markdown to an MS Word document.
#'
#' @inheritParams pdf_document
#' @inheritParams html_document
#'
#' @param reference_docx Use the specified file as a style reference in
#'   producing a docx file. For best results, the reference docx should be a
#'   modified version of a docx file produced using pandoc. Pass "default"
#'   to use the rmarkdown default styles.
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' See the \href{http://rmarkdown.rstudio.com/word_document_format.html}{online
#' documentation} for additional details on using the \code{word_document} format.
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
#' render("input.Rmd", word_document())
#'
#' # specify an option for syntax highlighting
#' render("input.Rmd", word_document(highlight = "zenburn"))
#' }
#'
#' @export
word_document <- function(toc = FALSE,
                          toc_depth = 3,
                          fig_width = 5,
                          fig_height = 4,
                          fig_caption = TRUE,
                          highlight = "default",
                          reference_docx = "default",
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

  # base pandoc options for all docx output
  args <- c()

  # table of contents
  if (pandoc_available("1.14"))
    args <- c(args, pandoc_toc_args(toc, toc_depth))
  else
    warning("table of contents for word_document requires pandoc >= 1.14")

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))

  # reference docx
  if (!is.null(reference_docx) && !identical(reference_docx, "default")) {
    args <- c(args, "--reference-docx", pandoc_path_arg(reference_docx))
  }

  # pandoc args
  args <- c(args, pandoc_args)

  # return output format
  output_format(
    knitr = knitr,
    pandoc = pandoc_options(to = "docx",
                            from = from_rmarkdown(fig_caption, md_extensions),
                            args = args),
    keep_md = keep_md
  )
}



