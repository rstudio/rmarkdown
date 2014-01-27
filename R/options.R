
#' Available syntax highlighers.
#'
#' Get the available syntax highlighters (supported highlighers are "default",
#' "pygments", "kate", "monochrome", "espresso", "zenburn", "haddock", and
#' "tango").
#'
#' @return Character vector with available syntax highlighers
#'
#' @details This is a convenience method for use in the signatures of options
#' functions (e.g. \code{\link{htmlOptions}} or \code{\link{pdfOptions}}).
#'
#' @export
highlighter <- function() {
  c("default",
    "pygments",
    "kate",
    "monochrome",
    "espresso",
    "zenburn",
    "haddock",
    "tango")
}
