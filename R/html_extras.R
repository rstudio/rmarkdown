
# resolve the html extras for a document (dependencies and arbitrary html to
# inject into the document)
html_extras_for_document <- function(knit_meta, runtime, dependency_resolver,
                                     format_deps = NULL) {

  extras <- list()

  # merge the dependencies discovered with the dependencies of this format and
  # dependencies discovered in knit_meta
  all_dependencies <- if (is.null(format_deps)) list() else format_deps
  all_dependencies <- append(all_dependencies, flatten_html_dependencies(knit_meta))
  extras$dependencies <- dependency_resolver(all_dependencies)

  # return extras
  extras
}

# convert html extras to the pandoc args required to include them
pandoc_html_extras_args <- function(extras, self_contained, lib_dir,
                                    output_dir, allow_uptree_lib_dir) {

  args <- c()

  # dependencies
  dependencies <- extras$dependencies
  if (length(dependencies) > 0) {
    if (self_contained)
      file <- as_tmpfile(html_dependencies_as_string(dependencies, NULL, NULL,
                                                     allow_uptree_lib_dir = FALSE))
    else
      file <- as_tmpfile(html_dependencies_as_string(dependencies, lib_dir,
                                                     output_dir,
                                                     allow_uptree_lib_dir))
    args <- c(args, pandoc_include_args(in_header = file))
  }

  # extras
  args <- c(args, pandoc_include_args(
                    in_header = as_tmpfile(extras$in_header),
                    before_body = as_tmpfile(extras$before_body),
                    after_body = as_tmpfile(extras$after_body)))

  args
}
