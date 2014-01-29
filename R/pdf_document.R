#' Convert to a PDF document
#'
#' Format for converting from R Markdown to a PDF document.
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param number.sections \code{TRUE} Number section headings
#' @param fig.width Default width (in inches) for figures
#' @param fig.height Default width (in inches) for figures
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "pygments", "kate", "monochrome", "espresso", "zenburn",
#'   "haddock", and "tango". Pass \code{NULL} to prevent syntax highlighting.
#'   Pass \code{NULL} to prevent syntax highlighting.
#' @param latex.engine LaTeX engine for producing PDF output. Options are
#'   "pdflatex", "lualatex", and "xelatex".
#' @param natbib Use natbib for citations in LaTeX output
#' @param biblatex Use biblatex for citations in LaTeX output
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link[pandoc:include_options]{include_options}}
#'   function).
#' @param pandoc.args Additional command line options to pass to pandoc
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' Creating PDF output from R Markdown requires that LaTeX be installed.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. Metadata can
#' also be provided to enable the use of footnotes and bibliographies.
#' For more details see the documentation on R Markdown
#' \link[=rmd_metadata]{metadata} and \link[=rmd_citations]{citations}.
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
#'    \item{\code{mainfont, sansfont, monofont, mathfont}}{Document fonts (works only with xelatex and lualatex, see the \code{latex.engine} option)}
#'    \item{\code{linkcolor, urlcolor, citecolor}}{Color for internal, external, and citation links (red, green, magenta, cyan, blue, black)}
#'    \item{\code{biblio-style}}{LaTeX bibliography style (used with \code{natbib} option)}
#'    \item{\code{biblio-files}}{Bibliography files to use in LaTeX (used with \code{natbib} or \code{biblatex} options)}
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
#' render("input.Rmd", pdf_document(latex.engine = "lualatex"))
#'
#' # add a table of contents and pass an option to pandoc
#' render("input.Rmd", pdf_document(toc = TRUE, "--listings"))
#' }
#'
#' @export
pdf_document <- function(toc = FALSE,
                         toc.depth = 2,
                         number.sections = FALSE,
                         fig.width = 6,
                         fig.height = 4.5,
                         highlight = "default",
                         latex.engine = "pdflatex",
                         natbib = FALSE,
                         biblatex = FALSE,
                         includes = NULL,
                         pandoc.args = NULL) {

  # knitr options
  knitr <- knitr_options(
    knit_hooks = list(crop = knitr::hook_pdfcrop),
    opts_chunk = list(dev = 'cairo_pdf',
                      fig.width = fig.width,
                      fig.height = fig.height)
  )

  # base pandoc options for all PDF output
  args <- c()

  # table of contents
  args <- c(args, pandoc::toc_options(toc, toc.depth))

  # template path and assets
  args <- c(args,
            pandoc::template_options(pandoc_template("latex/default.tex")))

  # numbered sections
  if (number.sections)
    args <- c(args, "--number-sections")

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight, highlighters())
  args <- c(args, pandoc::highlight_options(highlight))

  # latex engine
  latex.engine = match.arg(latex.engine, c("pdflatex", "lualatex", "xelatex"))
  args <- c(args, "--latex-engine", latex.engine)

  # natbib
  if (natbib)
    args <- c(args, "--natbib")

  # biblatex
  if (biblatex)
    args <- c(args, "--biblatex")

  # content includes
  args <- c(args, includes)

  # args args
  args <- c(args, pandoc.args)

  # return format
  output_format(
    knitr = knitr,
    pandoc = pandoc_options(to = "latex", args = args),
    filter = filter_pdf
  )
}


# Use filter to set pdf geometry defaults (while making sure we don't override
# any geometry settings already specified by the user)
filter_pdf <- function(format, input) {

  # set the margin to 1 inch if not otherwise specified
  has_margin <- function(text) {
    length(grep("^geometry\\:[ \\t]*margin=\\d+(\\.?\\d+)?\\w+$", text)) > 0
  }
  if (!has_margin(input) && !has_margin(format$pandoc))
    format$pandoc <- c(format$pandoc, "--variable", "geometry:margin=1in")

  format
}
