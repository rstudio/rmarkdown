#' Convert R Markdown to MS Word
#'
#' Converts the input file to MS Word docx using pandoc. If the input requires
#' knitting then \code{\link[knitr:knit]{knit}} is called prior to pandoc.
#'
#' @param input Input file (Rmd or plain markdown)
#' @param options Character vector of pandoc options created by calling
#'   \code{\link{docxOptions}}
#' @param output Target output file (defaults to <input>.docx if not specified)
#' @param envir The environment in which the code chunks are to be evaluated
#'   during knitting (can use \code{\link{new.env}()} to guarantee an empty new
#'   environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @details R Markdown documents can have optional metadata that is used to
#'   generate a document header that includes the title, author, and date.
#'   Metadata can also be provided to enable the use of footnotes and
#'   bibliographies. For more details see the documentation on
#'   \link[=rmdMetadata]{R Markdown Metadata}.
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

  # knitr rendering
  if (knitRequired(input))
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
  options <- c(options, pandoc::highlightOptions(highlight))

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



