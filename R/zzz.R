.onLoad <- function(pkg, lib) {
  assign(".render_context", new_stack(), envir = asNamespace("rmarkdown"))
}
