knit_print.data.frame <- function(x, ...) {
  context <- render_context()
  if (!is.null(context$df_print)) {
    if (identical(context$df_print, knitr::kable)) {
      res = paste(c('<div class="kable-table">', '', knitr::kable(x), '', '</div>'),
                  collapse = "\n")
      knitr::asis_output(res)
    } else {
      context$df_print(x)
    }
  } else {
    print(x, ...)
  }
}

#' @export
#' @importFrom knitr knit_print
knit_print.tbl_sql <- knit_print.data.frame

#' @export
knit_print.grouped_df <- knit_print.data.frame

#' @export
knit_print.rowwise_df <- knit_print.data.frame

