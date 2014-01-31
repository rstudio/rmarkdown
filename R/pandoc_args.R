#' Functions for generating pandoc command line arguments
#'
#' Functions that assist in creating various types of pandoc command line
#' arguments (e.g. for templates, table of contents, highlighting, and content
#' includes)
#'
#' @inheritParams includes
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param template Path to a pandoc template to use for conversion (should be
#'   either absolute or realtive to the directory of the input file).
#' @param highlight The name of a pandoc syntax highlighting theme.
#'
#' @return A character vector with pandoc command line arguments
#'
#' @details Non-absolute paths for resources referenced from the
#'   \code{in.header}, \code{before.body}, and \code{after.body}
#'   parameters are resolved relative to the directory of the input document.
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' pandoc_include_args(before.body = "header.htm")
#' pandoc_include_args(before.body = "header.tex")
#'
#' pandoc_template_args("custom.htm")
#'
#' pancoc_highlight_args("kate")
#'
#' pandoc_toc_args(toc = TRUE, toc.depth = 2)
#' }
#' @name pandoc_args
NULL


#' @rdname pandoc_args
#' @export
pandoc_include_args <- function(in.header = NULL,
                                before.body = NULL,
                                after.body = NULL) {
  args <- c()

  for (file in in.header)
    args <- c(args, "--include-in-header", file)

  for (file in before.body)
    args <- c(args, "--include-before-body", file)

  for (file in after.body)
    args <- c(args, "--include-after-body", file)

  args
}


#' @rdname pandoc_args
#' @export
pandoc_template_args <- function(template) {
  if (file.exists(template))
    template <- tools::file_path_as_absolute(template)
  c("--template", template,
    "--data-dir", dirname(template))
}

#' @rdname pandoc_args
#' @export
pandoc_highlight_args <- function(highlight) {

  args <- c()

  if (is.null(highlight))
    args <- c(args, "--no-highlight")
  else {
    if (identical(highlight, "default"))
      highlight <- "pygments"
    args <- c(args, "--highlight-style", highlight)
  }

  args
}

#' @rdname pandoc_args
#' @export
pandoc_toc_args <- function(toc, toc.depth = 3) {

  args <- c()

  if (toc) {
    args <- c(args, "--table-of-contents")
    args <- c(args, "--toc-depth", toc.depth)
  }

  args
}
