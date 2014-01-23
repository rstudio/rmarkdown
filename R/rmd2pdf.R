#' Convert R Markdown to PDF
#'
#' Converts an R Markdown (Rmd) file to PDF
#'
#' @param input Input Rmd document
#' @param options Character vector of pandoc options created by calling
#'   \code{pdfOptions}
#' @param output Target output file (defaults to <input>.pdf if not specified)
#' @param envir The environment in which the code chunks are to be evaluated
#'   (can use \code{\link{new.env}()} to guarantee an empty new environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @export
rmd2pdf <- function(input,
                    options = pdfOptions(),
                    output = NULL,
                    envir = parent.frame(),
                    quiet = FALSE,
                    encoding = getOption("encoding")) {

  # knitr options
  knitrRenderPDF("latex", 7, 7)

  # call pandoc
  rmd2pandoc(input, "latex", options, output, envir, quiet, encoding)
}



#' @rdname knitrRender
#' @export
knitrRenderPDF <- function(format, fig.width, fig.height) {

  # inherit defaults
  knitrRender(format)

  # crop
  knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)

  # graphics device
  knitr::opts_chunk$set(dev = 'cairo_pdf',
                        fig.width = fig.width,
                        fig.height = fig.height)
}


#' Options for PDF conversion
#'
#' Define the options for converting R Markdown to PDF.
#'
#' @param \dots Command line options to pass to pandoc
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param number.sections \code{TRUE} Number section headings
#' @param geometry Named LaTeX geometry options for the document.
#' @param highlight Style for syntax highlighting. Options are default,
#'   pygments, kate, monochrome, espresso, zenburn, haddock, and tango. Pass
#'   \code{NULL} to prevent syntax highlighting.
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link{includeOptions}} function).
#'
#' @return A character vector of PDF options that can be passed to
#'   \code{\link{rmd2pdf}}.
#'
#' @export
pdfOptions <- function(...,
                       toc = FALSE,
                       toc.depth = 2,
                       number.sections = FALSE,
                       geometry = c(margin = "1in"),
                       highlight = "default",
                       includes = NULL) {

  # base options for all PDF output
  options <- c()

  # table of contents
  options <- c(options, tableOfContentsOptions(toc, toc.depth))

  # numbered sections
  if (number.sections)
    options <- c(options, "--number-sections")

  # geometry
  for (name in names(geometry)) {
    value <- geometry[[name]]
    options <- c(options,
              "--variable",
              paste0("geometry:", name, "=", value))
  }

  # highlighting
  options <- c(options, highlightOptions(highlight))

  # content includes
  options <- c(options, includes)

  # dots
  options <- c(options, as.character(list(...)))

  options
}
