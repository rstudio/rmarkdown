#' Include content within output
#'
#' Specify additional content to be included within an output document.
#'
#' @param in.header One or more files with content to be included in the
#'   header of the document.
#' @param before.body One or more files with content to be included before
#'   the document body.
#' @param after.body One or more files with content to be included after the
#'   document body.
#'
#' @return A list with content to be included in output.
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
#' html_document(includes = includes(before.body = "header.htm"))
#'
#' pdf_document(includes = includes(after.body = "footer.tex"))
#'
#' }
#' @name includes
#' @export
includes <- function(in.header = NULL,
                     before.body = NULL,
                     after.body = NULL) {
  list(in.header = in.header,
       before.body = before.body,
       after.body = after.body)
}


includes_to_pandoc_args <- function(includes) {
  if (!is.null(includes))
    pandoc_include_args(in.header = includes$in.header,
                        before.body = includes$before.body,
                        after.body = includes$after.body)
  else
    NULL
}

