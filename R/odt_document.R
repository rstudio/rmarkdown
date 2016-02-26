#' Convert to an OpenDocument Text (ODT) document
#'
#' Format for converting from R Markdown to an ODT document.
#'
#' @inheritParams pdf_document
#' @inheritParams html_document
#'
#' @param reference_odt Use the specified file as a style reference in
#'   producing an odt file. For best results, the reference odt should be a
#'   modified version of an odt file produced using pandoc. Pass "default"
#'   to use the rmarkdown default styles.
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' See the \href{http://rmarkdown.rstudio.com/odt_document_format.html}{online
#' documentation} for additional details on using the \code{odt_document} format.
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
#' render("input.Rmd", odt_document())
#'
#' # specify an option for syntax highlighting
#' render("input.Rmd", odt_document(highlight = "zenburn"))
#' }
#'
#' @export
odt_document <- function(fig_width = 5,
                         fig_height = 4,
                         fig_caption = TRUE,
                         template = "default",
                         reference_odt = "default",
                         includes = NULL,
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

  # base pandoc options for all odt output
  args <- c()

  # template
  if (!is.null(template) && !identical(template, "default"))
    args <- c(args, "--template", pandoc_path_arg(template))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # reference odt
  if (!is.null(reference_odt) && !identical(reference_odt, "default")) {
    args <- c(args, "--reference-odt", pandoc_path_arg(reference_odt))
  }

  # pandoc args
  args <- c(args, pandoc_args)

  # return output format
  output_format(
    knitr = knitr,
    pandoc = pandoc_options(to = "odt",
                            from = from_rmarkdown(fig_caption, md_extensions),
                            args = args),
    keep_md = keep_md
  )
}



