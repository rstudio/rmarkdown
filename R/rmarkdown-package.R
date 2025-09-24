#' R Markdown Document Conversion
#'
#' Convert R Markdown documents into a variety of formats including HTML,
#' MS Word, PDF, and Beamer.
#' @details The \pkg{rmarkdown} package includes high level functions for
#'   converting to a variety of formats. For example:
#'   \preformatted{
#' render("input.Rmd", html_document())
#' render("input.Rmd", pdf_document())
#' }
#'   You can also specify a plain markdown file in which case knitting will be
#'   bypassed:
#'   \preformatted{render("input.md", html_document())}
#'   Additional options can be specified along with the output format:
#'   \preformatted{render("input.Rmd", html_document(toc = TRUE))
#' render("input.Rmd", pdf_document(latex_engine = "lualatex"))
#' render("input.Rmd", beamer_presentation(incremental = TRUE))
#' }
#'   You can also include arbitrary pandoc command line arguments along with the
#'   other options:
#'   \preformatted{
#' render("input.Rmd", pdf_document(toc = TRUE, pandoc_args = "--listings"))
#' }
#' @seealso \link{render}, \link{html_document}, \link{pdf_document},
#'   \link{word_document}, \link{beamer_presentation}
#' @name rmarkdown-package
#' @aliases rmarkdown
"_PACKAGE"
