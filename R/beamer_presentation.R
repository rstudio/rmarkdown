#' Convert to a Beamer presentation
#'
#' Format for converting from R Markdown to a Beamer presentation.
#'
#' @inheritParams pdf_document
#' @inheritParams html_document
#'
#' @param toc \code{TRUE} to include a table of contents in the output (only
#'   level 1 headers will be included in the table of contents).
#' @param slide_level The heading level which defines indvidual slides. By
#'   default this is the highest header level in the hierarchy that is followed
#'   immediately by content, and not another header, somewhere in the document.
#'   This default can be overridden by specifying an explicit
#'   \code{slide.level}.
#' @param incremental \code{TRUE} to render slide bullets incrementally. Note
#'   that if you want to reverse the default incremental behavior for an
#'   individual bullet you can preceded it with \code{>}. For example:
#'   \emph{\code{> - Bullet Text}}
#' @param theme Beamer theme (e.g. "AnnArbor").
#' @param colortheme Beamer color theme (e.g. "dolphin").
#' @param fonttheme Beamer font theme (e.g. "structurebold").
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' See the \href{http://rmarkdown.rstudio.com/beamer_presentation_format.html}{online
#' documentation} for additional details on using the \code{beamer_presentation} format.
#'
#' Creating Beamer output from R Markdown requires that LaTeX be installed.
#'
#' For more information on markdown syntax for presentations see
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
#' slide shows with pandoc}.
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
#' render("pres.Rmd", beamer_presentation())
#'
#' # specify an option for incremental rendering
#' render("pres.Rmd", beamer_presentation(incremental = TRUE))
#' }
#'
#' @export
beamer_presentation <- function(toc = FALSE,
                                slide_level = NULL,
                                incremental = FALSE,
                                fig_width = 10,
                                fig_height = 7,
                                fig_crop = TRUE,
                                fig_caption = FALSE,
                                dev = 'pdf',
                                theme = "default",
                                colortheme = "default",
                                fonttheme = "default",
                                highlight = "default",
                                template = "default",
                                keep_tex = FALSE,
                                latex_engine = "pdflatex",                              
                                includes = NULL,
                                md_extensions = NULL,
                                pandoc_args = NULL) {

  # base pandoc options for all beamer output
  args <- c()

  # template path and assets
  if (!is.null(template) && !identical(template, "default"))
    args <- c(args, "--template", pandoc_path_arg(template))

  # table of contents
  if (toc)
    args <- c(args, "--table-of-contents")

  # slide level
  if (!is.null(slide_level))
    args <- c(args, "--slide-level", as.character(slide_level))

  # incremental
  if (incremental)
    args <- c(args, "--incremental")

  # themes
  if (!identical(theme, "default"))
    args <- c(args, pandoc_variable_arg("theme", theme))
  if (!identical(colortheme, "default"))
    args <- c(args, pandoc_variable_arg("colortheme", colortheme))
  if (!identical(fonttheme, "default"))
    args <- c(args, pandoc_variable_arg("fonttheme", fonttheme))

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))

  # latex engine
  latex_engine = match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))
  
  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # custom args
  args <- c(args, pandoc_args)

  # return format
  output_format(
    knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev),
    pandoc = pandoc_options(to = "beamer",
                            from = from_rmarkdown(fig_caption, md_extensions),
                            args = args,
                            keep_tex = keep_tex),
    clean_supporting = !keep_tex
  )
}



