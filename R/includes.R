#' Include content within output
#'
#' Specify additional content to be included within an output document.
#'
#' Non-absolute paths for resources referenced from the
#' \code{in_header}, \code{before_body}, and \code{after_body}
#' parameters are resolved relative to the directory of the input document.
#' @param in_header One or more files with content to be included in the
#'   header of the document.
#' @param before_body One or more files with content to be included before
#'   the document body.
#' @param after_body One or more files with content to be included after the
#'   document body.
#' @param includes Includes to convert to pandoc args.
#' @param filter Filter to pre-process includes with.
#' @return Includes list or pandoc args
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' html_document(includes = includes(before_body = "header.htm"))
#'
#' pdf_document(includes = includes(after_body = "footer.tex"))
#' }
#' @name includes
#' @export
includes <- function(in_header = NULL,
                     before_body = NULL,
                     after_body = NULL) {

  list(in_header = in_header,
       before_body = before_body,
       after_body = after_body)
}


#' @rdname includes
#' @export
includes_to_pandoc_args <- function(includes,
                                    filter = identity) {
  if (!is.null(includes))
    pandoc_include_args(in_header = filter(includes$in_header),
                        before_body = filter(includes$before_body),
                        after_body = filter(includes$after_body))
}


# simple wrapper over normalizePath that preserves NULLs and applies pandoc-
# friendly defaults
normalize_path <- function(path, winslash = "/", must_work = NA) {
  if (!is.null(path)) normalizePath(path, winslash = winslash, mustWork = must_work)
}
