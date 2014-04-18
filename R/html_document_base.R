# base for HTML-based output formats
html_document_base <- function(smart = TRUE) {
  args <- c()

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  output_format(
    knitr = NULL,
    pandoc = pandoc_options(to = "html", from = NULL, args = args),
    clean_supporting = FALSE,
    pre_processor = NULL,
    post_processor = NULL
  )
}
