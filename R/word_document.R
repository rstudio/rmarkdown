#' Convert to an MS Word document
#'
#' Format for converting from R Markdown to an MS Word document.
#'
#' See the \href{https://bookdown.org/yihui/rmarkdown/word-document.html}{online
#' documentation} for additional details on using the \code{word_document} format.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations in the
#' \href{https://pandoc.org/MANUAL.html#citations}{Bibliographies
#' and Citations} article in the online documentation.
#' @inheritParams pdf_document
#' @inheritParams html_document
#' @param reference_docx Use the specified file as a style reference in
#'   producing a docx file. For best results, the reference docx should be a
#'   modified version of a docx file produced using pandoc. Pass "default"
#'   to use the rmarkdown default styles.
#' @return R Markdown output format to pass to \code{\link{render}}
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' # simple invocation
#' render("input.Rmd", word_document())
#'
#' # specify an option for syntax highlighting
#' render("input.Rmd", word_document(highlight = "zenburn"))
#' }
#' @export
word_document <- function(toc = FALSE,
                          toc_depth = 3,
                          number_sections = FALSE,
                          fig_width = 5,
                          fig_height = 4,
                          fig_caption = TRUE,
                          df_print = "default",
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
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # Lua filters (added if pandoc > 2)
  lua_filters <- pkg_file_lua("pagebreak.lua")

  # numbered sections
  if (number_sections) {
    if (pandoc_available("2.10.1")) {
      args <- c(args, "--number-sections")
    } else {
      lua_filters <- c(lua_filters, pkg_file_lua("number-sections.lua"))
    }
  }

  # highlighting
  if (!is.null(highlight)) highlight <- resolve_highlight(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))

  # reference docx
  args <- c(args, reference_doc_args("docx", reference_docx))

  # pandoc args
  args <- c(args, pandoc_args)

  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    NULL
  }

  intermediates_generator <- function(...) {
    reference_intermediates_generator(saved_files_dir, ..., reference_docx)
  }

  # return output format
  output_format(
    knitr = knitr,
    pandoc = pandoc_options(to = "docx",
                            from = from_rmarkdown(fig_caption, md_extensions),
                            args = args,
                            lua_filters = lua_filters),
    keep_md = keep_md,
    df_print = df_print,
    pre_processor = pre_processor,
    intermediates_generator = intermediates_generator
  )
}

reference_doc_args <- function(type, doc) {
  if (is.null(doc) || identical(doc, "default")) return()
  c(paste0("--reference-", if (pandoc2.0()) "doc" else {
    match.arg(type, c("docx", "odt", "doc"))
  }), pandoc_path_arg(doc))
}

