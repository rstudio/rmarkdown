#' Convert to a PDF document
#'
#' Format for converting from R Markdown to a PDF document.
#'
#' @inheritParams html_document
#'
#' @param fig_crop \code{TRUE} to automatically apply the \code{pdfcrop} utility
#'   (if available) to pdf figures
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "tango", "pygments", "kate", "monochrome", "espresso",
#'   "zenburn", and "haddock". Pass \code{NULL} to prevent syntax highlighting.
#' @param keep_tex Keep the intermediate tex file used in the conversion to PDF
#' @param latex_engine LaTeX engine for producing PDF output. Options are
#'   "pdflatex", "lualatex", and "xelatex".
#' @param template Pandoc template to use for rendering. Pass "default" to use
#'   the rmarkdown package default template; pass \code{NULL} to use pandoc's
#'   built-in template; pass a path to use a custom template that you've
#'   created.  See the documentation on
#'   \href{http://johnmacfarlane.net/pandoc/demo/example9/templates.html}{pandoc
#'   templates} for more details.
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' Creating PDF output from R Markdown requires that LaTeX be installed.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations within the pandoc documentation on
#' \href{http://johnmacfarlane.net/pandoc/demo/example19/Citations.html}{citations}
#' and
#' \href{http://johnmacfarlane.net/pandoc/demo/example19/Footnotes.html}{footnotes}.
#'
#' Many aspects of the LaTeX template used to create PDF documents can be
#' customized using metadata. For example:
#'
#' \tabular{l}{
#' \code{---} \cr
#' \code{title: "Crop Analysis Q3 2013"} \cr
#' \code{fontsize: 11pt} \cr
#' \code{geometry: margin=1in} \cr
#' \code{---}
#' }
#'
#' Available metadata variables include:
#'
#' \describe{
#'    \item{\code{lang}}{Document language code}
#'    \item{\code{fontsize}}{Font size (e.g. 10pt, 11pt, 12pt)}
#'    \item{\code{documentclass}}{LaTeX document class (e.g. article)}
#'    \item{\code{classoption}}{Option for \code{documentclass} (e.g. oneside); may be repeated}
#'    \item{\code{geometry}}{Options for geometry class (e.g. margin=1in); may be repeated}
#'    \item{\code{mainfont, sansfont, monofont, mathfont}}{Document fonts (works only with xelatex and lualatex, see the \code{latex_engine} option)}
#'    \item{\code{linkcolor, urlcolor, citecolor}}{Color for internal, external, and citation links (red, green, magenta, cyan, blue, black)}
#'    \item{\code{setspace}}{Options for line spacing (e.g. onehalfspacing, doublespacing)}
#' }
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' # simple invocation
#' render("input.Rmd", pdf_document())
#'
#' # specify an option for latex engine
#' render("input.Rmd", pdf_document(latex_engine = "lualatex"))
#'
#' # add a table of contents and pass an option to pandoc
#' render("input.Rmd", pdf_document(toc = TRUE, "--listings"))
#' }
#'
#' @export
pdf_document <- function(toc = FALSE,
                         toc_depth = 2,
                         number_sections = FALSE,
                         fig_width = 6.5,
                         fig_height = 4.5,
                         fig_crop = TRUE,
                         fig_caption = FALSE,
                         highlight = "default",
                         template = "default",
                         keep_tex = FALSE,
                         latex_engine = "pdflatex",
                         includes = NULL,
                         pandoc_args = NULL) {

  # base pandoc options for all PDF output
  args <- c()

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # template path and assets
  if (identical(template, "default"))
    args <- c(args, "--template",
              pandoc_path_arg(rmarkdown_system_file("rmd/latex/default.tex")))
  else if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))

  # numbered sections
  if (number_sections)
    args <- c(args, "--number-sections")

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))

  # latex engine
  latex_engine = match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # args args
  args <- c(args, pandoc_args)

  # use a geometry filter when we are using the "default" template
  if (identical(template, "default"))
    pre_processor <- pdf_pre_processor
  else
    pre_processor <- NULL

  # return format
  output_format(
    knitr = knitr_options_pdf(fig_width, fig_height, fig_crop),
    pandoc = pandoc_options(to = "latex",
                            from = from_rmarkdown(fig_caption),
                            args = args,
                            keep_tex = keep_tex),
    clean_supporting = !keep_tex,
    pre_processor = pre_processor
  )
}


# Use filter to set pdf geometry defaults (while making sure we don't override
# any geometry settings already specified by the user)
pdf_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                              output_dir) {

  args <- c()

  # set the margin to 1 inch if no other geometry options specified
  has_geometry <- function(text) {
    length(grep("^geometry:.*$", text)) > 0
  }
  if (!has_geometry(readLines(input_file, warn = FALSE)))
    args <- c(args, "--variable", "geometry:margin=1in")

  args
}
