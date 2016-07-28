knit_print.data.frame = function(x, ...) {
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


