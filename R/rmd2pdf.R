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
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link{pandocIncludeOptions}} function).
#'
#' @return A list of PDF options that can be passed to \code{\link{rmd2pdf}}.
#'
#' @export
pdfOptions <- function(toc = FALSE,
                       toc.depth = 2,
                       number.sections = FALSE,
                       geometry = c(margin = "1in"),
                       highlight = "default",
                       includes = NULL) {
  structure(list(toc = toc,
                 toc.depth = toc.depth,
                 number.sections = number.sections,
                 geometry = geometry,
                 highlight = highlight,
                 includes = includes),
            class = "pdfOptions")
}

#' @S3method pandocOptions pdfOptions
pandocOptions.pdfOptions <- function(options) {

  # base options for all PDF output
  args <- c()

  # table of contents
  args <- c(args, pandocTableOfContentsOptions(options))

  # numbered sections
  if (options$number.sections)
    args <- c(args, "--number-sections")

  # geometry
  for (name in names(options$geometry)) {
    value <- options$geometry[[name]]
    args <- c(args,
              "--variable",
              paste0("geometry:", name, "=", value))
  }

  # highlighting
  args <- c(args, pandocHighlightOptions(options))

  # content includes
  args <- c(args, pandocOptions(options$includes))

  args
}
