#' Convert to a Beamer presentation
#'
#' Format for converting from R Markdown to a Beamer presentation.
#'
#' @inheritParams pdf_document
#' @inheritParams html_document
#'
#' @param toc \code{TRUE} to include a table of contents in the output (only
#'   level 1 headers will be included in the table of contents).
#' @param slide_level The heading level which defines individual slides. By
#'   default this is the highest header level in the hierarchy that is followed
#'   immediately by content, and not another header, somewhere in the document.
#'   This default can be overridden by specifying an explicit
#'   \code{slide_level}.
#' @param incremental \code{TRUE} to render slide bullets incrementally. Note
#'   that if you want to reverse the default incremental behavior for an
#'   individual bullet you can precede it with \code{>}. For example:
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
                                fig_caption = TRUE,
                                dev = 'pdf',
                                theme = "default",
                                colortheme = "default",
                                fonttheme = "default",
                                highlight = "default",
                                template = "default",
                                keep_tex = FALSE,
                                latex_engine = "pdflatex",
                                citation_package = c("none", "natbib", "biblatex"),
                                includes = NULL,
                                md_extensions = NULL,
                                pandoc_args = NULL) {

  # base pandoc options for all beamer output
  args <- c()

  # template path and assets
  if (!is.null(template)) {
    if (identical(template, "default")) template <- patch_beamer_template()
    if (!is.null(template))
      args <- c(args, "--template", pandoc_path_arg(template))
  }

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

  # citation package
  citation_package <- match.arg(citation_package)
  if (citation_package != "none") args <- c(args, paste0("--", citation_package))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # make sure the graphics package is always loaded
  if (identical(template, "default")) args <- c(args, "--variable", "graphics=yes")

  # custom args
  args <- c(args, pandoc_args)

  # return format
  output_format(
    knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev),
    pandoc = pandoc_options(to = "beamer",
                            from = from_rmarkdown(fig_caption, md_extensions),
                            args = args,
                            latex_engine = latex_engine,
                            keep_tex = keep_tex),
    clean_supporting = !keep_tex
  )
}

patch_beamer_template <- function() {
  if (pandoc_version() >= '1.15.2') return()  # no need to patch the template
  f <- tempfile(fileext = '.tex')
  command <- paste(quoted(pandoc()), "-D beamer >", quoted(f))
  with_pandoc_safe_environment({
    if (system(command) != 0) stop("Failed to execute the command '", command, "'")
  })
  patch <- c(
    "% Comment these out if you don't want a slide with just the",
    "% part/section/subsection/subsubsection title:", "\\AtBeginPart{",
    "  \\let\\insertpartnumber\\relax", "  \\let\\partname\\relax",
    "  \\frame{\\partpage}", "}", "\\AtBeginSection{",
    "  \\let\\insertsectionnumber\\relax", "  \\let\\sectionname\\relax",
    "  \\frame{\\sectionpage}", "}", "\\AtBeginSubsection{",
    "  \\let\\insertsubsectionnumber\\relax", "  \\let\\subsectionname\\relax",
    "  \\frame{\\subsectionpage}", "}"
  )
  tpl <- readLines(f, encoding = 'UTF-8')
  tpl <- sub(paste(patch, collapse = '\n'), '', paste(tpl, collapse = '\n'), fixed = TRUE)
  writeLines(enc2utf8(tpl), f, useBytes = TRUE)
  f
}
