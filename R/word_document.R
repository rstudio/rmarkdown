#' Convert to an MS Word document
#'
#' Format for converting from R Markdown to an MS Word document.
#'
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "pygments", "kate", "monochrome", "espresso", "zenburn",
#'   "haddock", and "tango". Pass \code{NULL} to prevent syntax highlighting.
#' @param reference.docx Use the specified file as a style reference in
#'   producing a docx file. For best results, the reference docx should be a
#'   modified version of a docx file produced using pandoc.
#' @param \dots Command line options to pass to pandoc
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
word_document <- function(...,
                          highlight = "default",
                          reference.docx = NULL) {

  # knitr options and hooks
  knitr <- list()
  knitr$opts_chunk = list(dev = 'png',
                          dpi = 300,
                          fig.width = 6,
                          fig.height = 4.5)

  # base pandoc options for all docx output
  pandoc <- c()

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight, highlighters())
  pandoc <- c(pandoc, pandoc::highlight_options(highlight))

  # reference docx
  if (!is.null(reference.docx)) {
    pandoc <- c(pandoc,
                 "--reference-docx",
                 tools::file_path_as_absolute(reference.docx))
  }

  # dots
  pandoc <- c(pandoc, as.character(list(...)))

  # return output format
  output_format(to = "docx",
                knitr = knitr,
                pandoc = pandoc)
}



