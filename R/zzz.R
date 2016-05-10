.onLoad <- function(lib, pkg) {
  assign(".render_context", new_stack(), envir = asNamespace("rmarkdown"))
}
