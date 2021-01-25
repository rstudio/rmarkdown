#' Convert to an OpenDocument Text (ODT) document
#'
#' Format for converting from R Markdown to an ODT document.
#'
#' See the \href{https://bookdown.org/yihui/rmarkdown/opendocument-text-document.html}{online
#' documentation} for additional details on using the \code{odt_document} format.
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
#' @inheritParams html_document
#' @param reference_odt Use the specified file as a style reference in
#'   producing an odt file. For best results, the reference odt should be a
#'   modified version of an odt file produced using pandoc. Pass "default"
#'   to use the rmarkdown default styles.
#' @return R Markdown output format to pass to \code{\link{render}}
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' # simple invocation
#' render("input.Rmd", odt_document())
#'
#' # specify an option for syntax highlighting
#' render("input.Rmd", odt_document(highlight = "zenburn"))
#' }
#' @export
odt_document <- function(number_sections = FALSE,
                         fig_width = 5,
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
  args <- c(args, reference_doc_args("odt", reference_odt))

  # pandoc args
  args <- c(args, pandoc_args)

  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    NULL
  }

  intermediates_generator <- function(...) {
    reference_intermediates_generator(saved_files_dir, ..., reference_odt)
  }

  # return output format
  output_format(
    knitr = knitr,
    pandoc = pandoc_options(
      to = "odt",
      from = from_rmarkdown(fig_caption, md_extensions),
      args = args,
      lua_filters = pkg_file_lua(
        c("pagebreak.lua", if (number_sections) "number-sections.lua"))
    ),
    keep_md = keep_md,
    pre_processor = pre_processor,
    intermediates_generator = intermediates_generator
  )
}
