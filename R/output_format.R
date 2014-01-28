#' Define an R Markdown output format
#'
#' Define an R Markdown output format based on a combination of knitr and pandoc
#' options.
#'
#' @param to Pandoc format to convert to
#' @param knitr List of knitr options and hooks. Valid list elements include
#'   \code{\link[knitr:opts_knit]{opts_knit}},
#'   \code{\link[knitr:opts_chunk]{opts_chunk}}, and
#'   \code{\link[knitr:knit_hooks]{knit_hooks}}.
#' @param pandoc Character vector of pandoc command line arguments
#' @param filter An optional filter function that receieves the format and lines
#'   of the input file as input and can return a modified format.
#'
#' @return An R Markdown output format definition that can be passed to
#'   \code{\link{render}}.
#'
#' @seealso \link{render}
#'
#' @export
output_format <- function(to, knitr, pandoc, filter = NULL) {
  structure(list(to = to,
                 knitr = knitr,
                 pandoc = pandoc,
                 filter = filter),
            class = "rmarkdown_output_format")
}




