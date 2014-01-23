
#' Functions for Generating Pandoc Options
#'
#' Functions that assist in creating various types of pandoc command line
#' options (e.g. for templates, table of contents, highlighting, and content
#' includes)
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param template Path to a pandoc template to use for conversion (should be
#'   either absolute or realtive to the directory of the input file).
#' @param highlight The name of a pandoc syntax highlighting theme.
#' @param header One or more files with content to be included in the
#'   header of the document.
#' @param before.body One or more files with content to be included before
#'   the document body.
#' @param after.body One or more files with content to be included after the
#'   document body.
#'
#' @return A character vector with pandoc command line arguments
#'
#' @details Non-absolute paths for resources referenced from the
#'   \code{include.header}, \code{include.before}, and \code{include.after}
#'   parameters are resolved relative to the directory of the input document.
#'
#' @name pandocOptions
#' @aliases includeOptions
#' @export
includeOptions <- function(header = NULL,
                           before.body = NULL,
                           after.body = NULL) {
  options <- c()

  for (file in header)
    options <- c(options, "--include-in-header", file)

  for (file in before.body)
    options <- c(options, "--include-before-body", file)

  for (file in after.body)
    options <- c(options, "--include-after-body", file)

  options
}


#' @rdname pandocOptions
#' @export
templateOptions <- function(template) {
  c("--template", template,
    "--data-dir", dirname(template))
}

#' @rdname pandocOptions
#' @export
highlightOptions <- function(highlight) {

  options <- c()

  if (is.null(highlight))
    options <- c(options, "--no-highlight")
  else {
    if (identical(highlight, "default"))
      highlight <- "pygments"
    options <- c(options, "--highlight-style", highlight)
  }

  options
}

#' @rdname pandocOptions
#' @export
tableOfContentsOptions <- function(toc, toc.depth) {

  options <- c()

  if (toc) {
    options <- c(options, "--table-of-contents")
    options <- c(options, "--toc-depth", toc.depth)
  }

  options
}
