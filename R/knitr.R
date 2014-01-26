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
#' @param format Pandoc format being rendered
#' @param fig.width Default width (in inches) for figures
#' @param fig.height Default height (in inches) for figures
#'
#' @details Some file formats like HTML and PDF have multiple potential pandoc
#'   formats (e.g. PDF can be either "latex" or "beamer" and HTML can be "html",
#'   "html5", "revealjs", etc.). The format argument is used to distinguish
#'   among these, as well as ensure that distinct formats have their own knitr
#'   figure directories.
#'
#'   You typically need to call only one knitr render function, as the various
#'   format-specific functions (e.g. \code{knitrRenderPDF}) all call the
#'   \code{knitrRender} function as part of their implementation.
#'
#' @export
knitrRender <- function(format) {

  # stock markdown options
  knitr::render_markdown()

  # chunk options
  knitr::opts_chunk$set(tidy = FALSE,    # don't reformat R code
                        error = FALSE)   # stop immediately on errors

  # figure directory scope
  knitr::opts_chunk$set(fig.path=paste("figure-", format, "/", sep = ""))
}

