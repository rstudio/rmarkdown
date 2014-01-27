#' Convert R Markdown to PDF
#'
#' Converts the input file to PDF using pandoc. If the input requires
#' knitting then \code{\link[knitr:knit]{knit}} is called prior to pandoc.
#'
#' @param input Input file (Rmd or plain markdown)
#' @param options Character vector of pandoc options created by calling
#'   \code{\link{pdfOptions}}
#' @param output Target output file (defaults to <input>.pdf if not specified)
#' @param envir The environment in which the code chunks are to be evaluated
#'   during knitting (can use \code{\link{new.env}()} to guarantee an empty
#'   new environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @details
#'
#' Creating PDF output from R Markdown requires that LaTeX be installed.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. Metadata can
#' also be provided to enable the use of footnotes and bibliographies.
#' For more details see the documentation on R Markdown
#' \link[=rmdMetadata]{metadata} and \link[=rmdCitations]{citations}.
#'
#' In addition to the options specified by \code{\link{pdfOptions}}, many other
#' aspects of the LaTeX template used to create PDF output can be customized
#' using metadata. For example:
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
#' @seealso \code{\link[knitr:knit]{knit}}, \code{\link{pdfOptions}}
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' # simple invocation
#' rmd2pdf("input.Rmd")
#'
#' # specify an option for latex engine
#' rmd2pdf("input.Rmd", pdfOptions(latex.engine = "lualatex"))
#'
#' # add a table of contents and pass an option to pandoc
#' rmd2pdf("input.Rmd", pdfOptions(toc = TRUE, "--listings"))
#' }
#'
#' @export
rmd2pdf <- function(input,
                    options = pdfOptions(),
                    output = NULL,
                    envir = parent.frame(),
                    quiet = FALSE,
                    encoding = getOption("encoding")) {

  # knitr rendering
  if (knitRequired(input))
    knitrRenderPdf("latex", 7, 7)

  # some special sauce to tweak the geometry if the user hasn't done
  # so explicitly within the document's metadata
  options <- c(options, pdfGeometry(readLines(input, warn = FALSE, encoding = encoding),
                                    options))

  # call pandoc
  rmd2pandoc(input, "latex", options, output, envir, quiet, encoding)
}



#' @rdname knitrRender
#' @export
knitrRenderPdf <- function(format, fig.width, fig.height) {

  # inherit defaults
  knitrRender(format)

  # crop
  knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)

  # graphics device
  knitr::opts_chunk$set(dev = 'cairo_pdf',
                        fig.width = fig.width,
                        fig.height = fig.height)
}


#' Options for PDF Conversion
#'
#' Define the options for converting R Markdown to PDF.
#'
#' @param \dots Command line options to pass to pandoc
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param number.sections \code{TRUE} Number section headings
#' @param highlight Syntax highlighting style (see \code{\link{highlighter}}).
#'   Pass \code{NULL} to prevent syntax highlighting.
#' @param latex.engine LaTeX engine for producing PDF output. Options are
#'   pdflatex, lualatex, and xelatex.
#' @param natbib Use natbib for citations in LaTeX output
#' @param biblatex Use biblatex for citations in LaTeX output
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link[pandoc:includeOptions]{includeOptions}}
#'   function).
#'
#' @return A character vector of PDF options that can be passed to
#'   \code{\link{rmd2pdf}}.
#'
#' @seealso \code{\link{rmd2pdf}}
#'
#' @export
pdfOptions <- function(...,
                       toc = FALSE,
                       toc.depth = 2,
                       number.sections = FALSE,
                       highlight = highlighter(),
                       latex.engine = c("pdflatex", "lualatex", "xelatex"),
                       natbib = FALSE,
                       biblatex = FALSE,
                       includes = NULL) {

  # base options for all PDF output
  options <- c()

  # table of contents
  options <- c(options, pandoc::tocOptions(toc, toc.depth))

  # template path and assets
  options <- c(options,
               pandoc::templateOptions(pandocTemplate("latex/default.tex")))

  # numbered sections
  if (number.sections)
    options <- c(options, "--number-sections")

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight)
  options <- c(options, pandoc::highlightOptions(highlight))

  # latex engine
  latex.engine = match.arg(latex.engine)
  options <- c(options, "--latex-engine", latex.engine)

  # natbib
  if (natbib)
    options <- c(options, "--natbib")

  # biblatex
  if (biblatex)
    options <- c(options, "--biblatex")

  # content includes
  options <- c(options, includes)

  # dots
  options <- c(options, as.character(list(...)))

  options
}


# helper function to set pdf geometry defaults (while making sure we
# don't override any geometry settings already specified by the user)
pdfGeometry <- function(inputText, options) {

  geometry <- c()

  # set the margin to 1 inch if not otherwise specified
  hasMargin <- function(text) {
    length(grep("^geometry\\:[ \\t]*margin=\\d+(\\.?\\d+)?\\w+$", text)) > 0
  }
  if (!hasMargin(inputText) && !hasMargin(options))
    geometry <- c(geometry, "--variable", "geometry:margin=1in")

  geometry
}
