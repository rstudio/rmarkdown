knit_print.data.frame <- function(x, ...) {
  context <- render_context()

  # printing of certain expressions producing
  # 'data.table' objects should be suppressed
  printable <- TRUE
  if ("data.table" %in% loadedNamespaces() &&
      exists("shouldPrint", envir = asNamespace("data.table")))
  {
    shouldPrint <- get("shouldPrint", envir = asNamespace("data.table"))
    printable <- tryCatch(
      shouldPrint(x) || !inherits(x, "data.table"),
      error = function(e) TRUE
    )
  }

  if (!printable)
    return(invisible(NULL))

  if (!is.null(context$df_print)) {
    if (identical(context$df_print, knitr::kable)) {
      res <- c('', knitr::kable(x), '')
      if (knitr::is_html_output())
        res <- c('<div class="kable-table">', res, '</div>')
      knitr::asis_output(one_string(res))
    } else {
      context$df_print(x)
    }
  } else {
    print(x)
  }
}

#' @export
#' @importFrom knitr knit_print
knit_print.tbl_sql <- knit_print.data.frame

#' @export
knit_print.grouped_df <- knit_print.data.frame

#' @export
knit_print.rowwise_df <- knit_print.data.frame

