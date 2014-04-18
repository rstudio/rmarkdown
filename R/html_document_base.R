# base for HTML-based output formats
html_document_base <- function(smart = TRUE,
                               theme = NULL,
                               self_contained = TRUE,
                               lib_dir = NULL,
                               mathjax = "default",
                               pandoc_args = NULL,
                               template = "default",
                               dependency_resolver = html_dependency_resolver,
                               ...) {
  args <- c()

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  # no email obfuscation
  args <- c(args, "--email-obfuscation", "none")

  # self contained document
  if (self_contained) {
    validate_self_contained(mathjax)
    args <- c(args, "--self-contained")
  }

  # custom args
  args <- c(args, pandoc_args)

  pre_processor <- function (metadata, input_lines, runtime, knit_meta,
                             files_dir) {

    args <- c()

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # handle theme
    if (!is.null(theme)) {
      theme <- match.arg(theme, themes())
      if (identical(theme, "default"))
        theme <- "bootstrap"
      args <- c(args, "--variable", paste("theme:", theme, sep=""))
    }

    # resolve and inject extras
    if (!is.null(theme))
      format_deps <- list(html_dependency_jquery(),
                          html_dependency_bootstrap(theme))
    else
      format_deps <- NULL
    extras <- html_extras_for_document(knit_meta, runtime, dependency_resolver,
                                       format_deps)
    args <- c(args, pandoc_html_extras_args(extras, self_contained, lib_dir))

    # mathjax
    args <- c(args, pandoc_mathjax_args(mathjax,
                                        template,
                                        self_contained,
                                        lib_dir))
    args
  }

  output_format(
    knitr = NULL,
    pandoc = pandoc_options(to = "html", from = NULL, args = args),
    clean_supporting = FALSE,
    pre_processor = pre_processor,
    post_processor = NULL
  )
}
