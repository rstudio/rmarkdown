#' Convert R Markdown to MS Word
#'
#' Converts an R Markdown (Rmd) file to an MS Word docx. The document is
#' \code{\link[knitr:knit]{knit}} and then converted to docx using
#' \href{http://johnmacfarlane.net/pandoc/index.html}{pandoc}.
#'
#' @param input input Rmd document
#' @param options Character vector of pandoc options created by calling
#'   \code{\link{docxOptions}}
#' @param output Target output file (defaults to <input>.docx if not specified)
#' @param envir The environment in which the code chunks are to be evaluated
#'   (can use \code{\link{new.env}()} to guarantee an empty new environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @section Metadata:
#'  Rmd files include a metadata section (typically located at the top of the file) that include title, author, and date information. Here is an example metadata section:
#'
#' \tabular{l}{
#' \code{---} \cr
#' \code{title: "Crop Analysis Q3 2013"} \cr
#' \code{author: Martha Smith} \cr
#' \code{date: October 23rd, 2013} \cr
#' \code{---}
#' }
#'
#' @seealso \code{\link[knitr:knit]{knit}}, \code{\link{docxOptions}},
#'
#' @export
rmd2docx <- function(input,
                     options = docxOptions(),
                     output = NULL,
                     envir = parent.frame(),
                     quiet = FALSE,
                     encoding = getOption("encoding")) {

  # knitr options
  knitrRenderDOCX("docx", 7, 7)

  # call pandoc
  rmd2pandoc(input, "docx", options, output, envir, quiet, encoding)
}


#' @rdname knitrRender
#' @export
knitrRenderDOCX <- function(format, fig.width, fig.height) {

  # inherit defaults
  knitrRender(format)

  # high resolution PNG
  knitr::opts_chunk$set(dev = 'png',
                        dpi = 300,
                        fig.width = fig.width,
                        fig.height = fig.height)
}



#' Options for MS Word conversion
#'
#' Define the options for converting R Markdown to MS Word docx
#'
#' @param \dots Command line options to pass to pandoc
#' @param highlight Style for syntax highlighting. Options are default,
#'   pygments, kate, monochrome, espresso, zenburn, haddock, and tango. Pass
#'   \code{NULL} to prevent syntax highlighting.
#' @param reference.docx Use the specified file as a style reference in
#'   producing a docx file. For best results, the reference docx should be a
#'   modified version of a docx file produced using pandoc.
#'
#' @return A character vector of options that can be passed to
#'   \code{\link{rmd2docx}}.
#'
#' @seealso \code{\link{rmd2docx}}
#'
#' @export
docxOptions <- function(...,
                        highlight = "default",
                        reference.docx = NULL) {

  # base options for all docx output
  options <- c()

  # highlighting
  options <- c(options, highlightOptions(highlight))

  # reference docx
  if (!is.null(reference.docx)) {
    options <- c(options,
                 "--reference-docx",
                 tools::file_path_as_absolute(reference.docx))
  }

  # dots
  options <- c(options, as.character(list(...)))

  options
}



