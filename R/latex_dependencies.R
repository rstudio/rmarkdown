#' Define a LaTeX package dependency
#' @param name The LaTeX package name
#' @param options The LaTeX options for the package
#' @param extra_lines LaTeX code related to the package added to the preamble
#' @export
latex_dependency <- function(name, options = NULL, extra_lines = NULL) {
  output <- list(name = name, options = options, extra_lines = extra_lines)
  class(output) <- "latex_dependency"
  validate_latex_dependency(output)
}

#' Provide common LaTeX dependencies
#'
#' These functions provide common LaTeX dependencies (e.g. tikz)
#' for R Markdown formats that use LaTeX.
#'
#' @inheritParams latex_dependency
#' @name latex-dependencies
NULL

# Create an LaTeX dependency for tikz
#' @rdname latex-dependencies
#' @param libraries A character vector of tikz libraries to load
#' @export
latex_dependency_tikz <- function(libraries, options = NULL, extra_lines = NULL) {
  libraries <- sprintf("\\usetikzlibrary{%s}", paste(libraries, collapse = ", "))
  latex_dependency("tikz", options = options, extra_lines = c(libraries, extra_lines))
}

latex_dependencies <- function(x = list()) {
  if (length(x) == 0) return()
  if (is_latex_dependency(x)) return(list(x))
  nms <- names(x)
  if (is.list(x)) {
    if (is.null(nms)) return(x)  # assume it is just a list of latex_dependency()
    # turn the named list to a named character vector
    x <- unlist(lapply(x, paste, collapse = ', '))
  }
  if (is.character(x)) {
    if (is.null(nms)) {
      lapply(x, latex_dependency)
    } else {
      mapply(latex_dependency, nms, x, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    }
  }
}

# return the LaTeX dependencies as a string suitable for inclusion
# in the head of a document
latex_dependencies_as_string <- function(dependencies) {
  lines <- sapply(dependencies, function(dep) {
    opts <- paste(dep$options, collapse = ",")
    if (opts != "") opts <- paste0("[", opts, "]")
    # \\usepackage[opt1,opt2]{pkgname}
    pkg <- paste0("\\usepackage", opts, "{", dep$name, "}")
    one_string(c(pkg, dep$extra_lines))
  })
  one_string(unique(lines))
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
    stop2("passed object is not of class latex_dependency")

  # validate required fields
  if (is.null(list$name))
    stop2("name (package name) for latex_dependency not provided")
  list
}

# check if the passed knit_meta has any latex dependencies
has_latex_dependencies <- function(knit_meta) {
  has_dependencies(knit_meta, "latex_dependency")
}
