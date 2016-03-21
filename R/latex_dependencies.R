#' Define a LaTeX package dependency
#' @param name The LaTeX package name
#' @param options The LaTeX options for the package
#' @export
latex_dependency <- function(name, options=NULL) {
  output <- list(name = name, options = options)
  class(output) <- "latex_dependency"
  return(validate_latex_dependency(output))
}


# Write the LaTeX dependencies to a text file, suitable for passing it
# to an include LaTeX command
latex_dependencies_as_text_file <- function(dependencies, filename) {
  text <- latex_dependencies_as_string(dependencies)
  cat(text, file = filename)
  return(filename)
}


# return the LaTeX dependencies as a string suitable for inclusion
# in the head of a document
latex_dependencies_as_string <- function(dependencies) {
  return(paste0(sapply(dependencies,
                       function(dep) { # \\usepackage[opt1,opt2]{pkgname}
                         paste0(c("\\usepackage[",
                                  paste(dep$options, collapse = ","),
                                  "]{", dep$name,"}"),
                                collapse = "")}),
                collapse = "\n"))
}



# flattens an arbitrarily nested list and returns all of the latex_dependency
# objects it contains
flatten_latex_dependencies <- function(knit_meta) {
  all_dependencies <- list()

  # knit_meta is a list of 'meta' attributes returned from custom knit_print
  # functions. since the 'meta' attribute could either be a latex dependency or
  # a list of dependencies we recurse on lists that aren't named
  for (dep in knit_meta) {
    if (is.null(names(dep)) && is.list(dep)) {
      inner_dependencies <- flatten_latex_dependencies(dep)
      all_dependencies <- append(all_dependencies, inner_dependencies)
    }
    else if (is_latex_dependency(dep)) {
      all_dependencies[[length(all_dependencies) + 1]] <- dep
    }
  }
  all_dependencies
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
  if (is.null(list$options))
    list$options = ""
  list
}

# check if the passed knit_meta has any html dependencies
has_latex_dependencies <- function(knit_meta) {

  if (inherits(knit_meta, "latex_dependency"))
    return(TRUE)

  else if (is.list(knit_meta)) {
    for (dep in knit_meta) {
      if (is.null(names(dep))) {
        if (has_latex_dependencies(dep))
          return(TRUE)
      } else {
        if (inherits(dep, "latex_dependency"))
          return(TRUE)
      }
    }

    FALSE
  } else {
    FALSE
  }
}
