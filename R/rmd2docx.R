#' Convert R Markdown to MS Word
#'
#' Converts an R Markdown (Rmd) file to an MS Word docx
#'
#' @param input input Rmd document
#' @param output Target output file (defaults to <input>.docx if not specified)
#' @param options List of MS Word rendering options created by calling
#'   \code{docxOptions}
#' @param envir The environment in which the code chunks are to be evaluated
#'   (can use \code{\link{new.env}()} to guarantee an empty new environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @export
rmd2docx <- function(input,
                     output = NULL,
                     options = docxOptions(),
                     envir = parent.frame(),
                     quiet = FALSE,
                     encoding = getOption("encoding")) {

  # knitr options
  knitrRenderDOCX("docx", 7, 7)

  # call pandoc
  rmd2pandoc(input, "docx", output, options, envir, quiet, encoding)
}


#' @rdname knitrRender
#' @export
knitrRenderDOCX <- function(format, fig.width, fig.height) {

  # inherit defaults
  knitrRender(format)

  # graphics device
  knitr::opts_chunk$set(dev = 'png',
                        fig.width = fig.width,
                        fig.height = fig.height)
}



#' Options for MS Word conversion
#'
#' Define the options for converting R Markdown to MS Word docx
#'
#' @return A list of options that can be passed to \code{\link{rmd2docx}}.
#'
#' @export
docxOptions <- function() {
  structure(list(),
            class = "docxOptions")
}


#' @S3method pandocOptions docxOptions
pandocOptions.docxOptions <- function(docxOptions) {

  # base options for all docx output
  options <- c()


  options
}



