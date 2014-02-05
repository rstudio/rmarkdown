#' Include content within output
#'
#' Specify additional content to be included within an output document.
#'
#' @param in_header One or more files with content to be included in the
#'   header of the document.
#' @param before_body One or more files with content to be included before
#'   the document body.
#' @param after_body One or more files with content to be included after the
#'   document body.
#'
#' @return A list with content to be included in output.
#'
#' @details Non-absolute paths for resources referenced from the
#'   \code{in_header}, \code{before_body}, and \code{after_body}
#'   parameters are resolved relative to the directory of the input document.
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' html_document(includes = includes(before_body = "header.htm"))
#'
#' pdf_document(includes = includes(after_body = "footer.tex"))
#'
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


includes_to_pandoc_args <- function(includes) {
  if (!is.null(includes))
    pandoc_include_args(in_header = includes$in_header,
                        before_body = includes$before_body,
                        after_body = includes$after_body)
  else
    NULL
}

