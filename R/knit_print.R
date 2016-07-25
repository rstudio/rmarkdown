paged_table_html = function(x) {
  paste(
    "<div data-pagedtable>",
    "  <script data-pagedtable-source type=\"application/json\">",
    jsonlite::toJSON(x),
    "  </script>",
    "</div>",
    sep = "\n"
  )
}

knit_print.data.frame = function(x, ...) {
  context <- render_context()
  if (!is.null(context$df_print)) {
    if (identical(context$df_print, knitr::kable)) {
      res = paged_table_html(x)
      knitr::asis_output(res)
    } else {
      context$df_print(x)
    }
  } else {
    print(x, ...)
  }
}


