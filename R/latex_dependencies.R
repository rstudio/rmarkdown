#' Define a LaTeX package dependency
#' @param name The LaTeX package name
#' @param options The LaTeX options for the package
#' @export
latex_dependency <- function(name, options = NULL) {
  output <- list(name = name, options = options)
  class(output) <- "latex_dependency"
  validate_latex_dependency(output)
}


# Write the LaTeX dependencies to a text file, suitable for passing it
# to an include LaTeX command
latex_dependencies_as_text_file <- function(dependencies, filename) {
  text <- latex_dependencies_as_string(dependencies)
  cat(text, file = filename)
  filename
}


# return the LaTeX dependencies as a string suitable for inclusion
# in the head of a document
latex_dependencies_as_string <- function(dependencies) {
  lines <- sapply(dependencies, function(dep) {
    opts <- paste(dep$options, collapse = ",")
    if (opts != "") opts <- paste0("[", opts, "]")
    # \\usepackage[opt1,opt2]{pkgname}
    paste0("\\usepackage", opts, "{", dep$name, "}")
  })
  paste(lines, collapse = "\n")
}


# flattens an arbitrarily nested list and returns all of the latex_dependency
# objects it contains
flatten_latex_dependencies <- function(knit_meta) {
  flatten_dependencies(knit_meta, is_latex_dependency)
}


# check class of passed list for 'latex_dependency'
is_latex_dependency <- function(list) {
  inherits(list, "latex_dependency")
}

# validate that the passed list is a correctly formed latex_dependency
validate_latex_dependency <- function(list) {

  # ensure it's the right class
  if (!is_latex_dependency(list))
    stop("passed object is not of class latex_dependency", call. = FALSE)

  # validate required fields
  if (is.null(list$name))
    stop("name (package name) for latex_dependency not provided", call. = FALSE)
  list
}

# check if the passed knit_meta has any latex dependencies
has_latex_dependencies <- function(knit_meta) {
  has_dependencies(knit_meta, "latex_dependency")
}
