#' Convert R Markdown to OpenDocument Text
#'
#' Converts an R Markdown (Rmd) file to an OpenDocument odt file
#'
#' @param input input Rmd document
#' @param options Character vector of pandoc options created by calling
#'   \code{odtOptions}
#' @param output Target output file (defaults to <input>.odt if not specified)
#' @param envir The environment in which the code chunks are to be evaluated
#'   (can use \code{\link{new.env}()} to guarantee an empty new environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @export
rmd2odt <- function(input,
                    options = odtOptions(),
                    output = NULL,
                    envir = parent.frame(),
                    quiet = FALSE,
                    encoding = getOption("encoding")) {

  # knitr options
  knitrRenderODT("odt", 7, 7)

  # call pandoc
  rmd2pandoc(input, "odt", options, output, envir, quiet, encoding)
}


#' @rdname knitrRender
#' @export
knitrRenderODT <- function(format, fig.width, fig.height) {

  # inherit defaults
  knitrRender(format)

  # graphics device
  knitr::opts_chunk$set(dev = 'png',
                        fig.width = fig.width,
                        fig.height = fig.height)
}


#' Options for OpenDocument conversion
#'
#' Define the options for converting R Markdown to OpenDocument odt
#'
#' @param highlight Style for syntax highlighting. Options are default,
#'   pygments, kate, monochrome, espresso, zenburn, haddock, and tango. Pass
#'   \code{NULL} to prevent syntax highlighting.
#' @param reference.odt Use the specified file as a style reference in producing
#'   an odt file. For best results, the reference odt should be a modified
#'   version of a odt file produced using pandoc.
#'
#' @return A character vector of options that can be passed to
#'   \code{\link{rmd2odt}}.
#'
#' @export
odtOptions <- function(highlight = "default",
                       reference.odt = NULL) {

  # base options for all odt output
  options <- c()

  # highlighting
  options <- c(options, highlightOptions(highlight))

  # reference docx
  if (!is.null(reference.odt)) {
    options <- c(options,
                 "--reference-odt",
                 tools::file_path_as_absolute(reference.odt))
  }

  options
}



