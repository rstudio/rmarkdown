#' Check Whether an Input Document Requires Knitting
#'
#' Check whether the input document requires knitting by evaluating it's
#' file extension. R Markdown (e.g. .Rmd) requires knitting whereas
#' plain markdown (e.g. .md) does not.
#'
#' @param input Input document
#'
#' @return Logical indicating whether the document requires knitting.
#'
#' @export
knitRequired <- function(input) {
  ext <- tolower(tools::file_ext(input))
  ext %in% c("rmd", "rmarkdown")
}


#' Set knitr Hooks and Options for Rendering R Markdown
#'
#' These functions set knitr hooks and options for markdown rendering. Hooks are
#' based on the default
#' \code{\link[knitr:render_markdown]{knitr::render_markdown}} function with
#' additional customization for various output formats.
#'
#' @param format Pandoc format being rendered (used to create distinct figure
#'   directories for multiple formats)
#' @param fig.width Default width (in inches) for figures
#' @param fig.height Default height (in inches) for figures
#'
#' @details You typically need to call only one knitr render function, as the
#' various format-specific functions (e.g. \code{knitrRenderPDF}) all call
#' the \code{knitrRender} function as part of their implementation.
#'
#' @export
knitrRender <- function(format) {

  # stock markdown options
  knitr::render_markdown()

  # chunk options
  knitr::opts_chunk$set(tidy = FALSE,    # don't reformat R code
                        comment = NA,    # don't preface output with ##
                        error = FALSE)   # stop immediately on errors

  # figure directory scope
  knitr::opts_chunk$set(fig.path=paste("figure-", format, "/", sep = ""))
}

