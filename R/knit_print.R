

knit_print.data.frame = function(x, ...) {
  context <- render_context()
  if (isTRUE(context$kable)) {
    res = paste(c('<div class="kable-table">', '', knitr::kable(x), '', '</div>'),
                collapse = "\n")
    knitr::asis_output(res)
  } else {
    print.data.frame(x, ...)
  }
}


