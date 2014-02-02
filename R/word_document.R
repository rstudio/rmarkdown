#' Convert to an MS Word document
#'
#' Format for converting from R Markdown to an MS Word document.
#'
#' @inheritParams pdf_document
#' @inheritParams html_document
#'
#' @param reference.docx Use the specified file as a style reference in
#'   producing a docx file. For best results, the reference docx should be a
#'   modified version of a docx file produced using pandoc.
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. Metadata can
#' also be provided to enable the use of footnotes and bibliographies.
#' For more details see the documentation on R Markdown
#' \link[=rmd_metadata]{metadata} and \link[=rmd_citations]{citations}.
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
word_document <- function(fig.width = 6,
                          fig.height = 4.5,
                          fig.caption = FALSE,
                          highlight = "default",
                          reference.docx = NULL,
                          data.dir = NULL,
                          pandoc.args = NULL) {

  # knitr options and hooks
  knitr <- knitr_options(
    opts_chunk = list(dev = 'png',
                      dpi = 300,
                      fig.width = fig.width,
                      fig.height = fig.height)
  )

  # base pandoc options for all docx output
  args <- c()

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))

  # reference docx
  if (!is.null(reference.docx)) {
    args <- c(args, "--reference-docx", pandoc_path(reference.docx))
  }

  # data dir
  if (!is.null(data.dir))
    args <- c(args, "--data-dir", pandoc_path(data.dir))

  # pandoc args
  args <- c(args, pandoc.args)

  # return output format
  output_format(
    knitr = knitr,
    pandoc = pandoc_options(to = "docx",
                            from_rmarkdown(fig.caption),
                            args = args)
  )
}



