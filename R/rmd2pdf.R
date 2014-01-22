#' Convert R Markdown to PDF
#'
#' Converts an R Markdown (Rmd) file to PDF
#'
#' @param input Input Rmd document
#' @param output Target output file (defaults to <input>.pdf if not specified)
#' @param options List of PDF rendering options created by calling
#'   \code{pdfOptions}
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
                    output = NULL,
                    options = pdfOptions(),
                    envir = parent.frame(),
                    quiet = FALSE,
                    encoding = getOption("encoding")) {

  # knitr options
  knitrRenderPDF("latex", 6, 5)

  # call pandoc
  rmd2pandoc(input, "latex", output, options, envir, quiet, encoding)
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
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param number.sections \code{TRUE} Number section headings
#' @param geometry Named LaTeX geometry options for the document.
#' @param highlight Style for syntax highlighting. Options are default,
#'   pygments, kate, monochrome, espresso, zenburn, haddock, and tango. Pass
#'   \code{NULL} to prevent syntax highlighting.
#' @param include.header One or more files with LaTeX content to be included in
#'   the header of the document.
#' @param include.before One or more files with LaTeX content to be included
#'   before the document body.
#' @param include.after One or more files with LaTeX content to be included
#'   after the document body.
#'
#' @details Paths for resources referenced from the \code{include.header},
#'   \code{include.before}, and \code{include.after} parameters are resolved
#'   relative to the directory of the input document.
#'
#' @return A list of PDF options that can be passed to \code{\link{rmd2pdf}}.
#'
#' @export
pdfOptions <- function(toc = FALSE,
                       toc.depth = 2,
                       number.sections = FALSE,
                       geometry = c(margin = "1in"),
                       highlight = "default",
                       include.header = NULL,
                       include.before = NULL,
                       include.after = NULL) {
  structure(list(toc = toc,
                 toc.depth = toc.depth,
                 number.sections = number.sections,
                 geometry = geometry,
                 highlight = highlight,
                 include.header = include.header,
                 include.before = include.before,
                 include.after = include.after),
            class = "pdfOptions")
}

#' @S3method pandocOptions pdfOptions
pandocOptions.pdfOptions <- function(pdfOptions) {

  # base options for all PDF output
  options <- c()

  # table of contents
  options <- c(options, pandocTableOfContentsOptions(pdfOptions))

  # numbered sections
  if (pdfOptions$number.sections)
    options <- c(options, "--number-sections")

  # geometry
  for (name in names(pdfOptions$geometry)) {
    value <- pdfOptions$geometry[[name]]
    options <- c(options,
                 "--variable",
                 paste0("geometry:", name, "=", value))
  }

  # highlighting
  options <- c(options, pandocHighlightOptions(pdfOptions))

  # content includes
  options <- c(options, pandocIncludeOptions(pdfOptions))

  options
}
