


# resolve dependencies for html_document
resolve_html_document_dependencies <- function(theme, knit_meta) {

  if (!is.null(theme))
    format_deps <- list(jquery_dependency(), bootstrap_dependency(theme))
  else
    format_deps <- NULL

  resolve_html_dependencies(format_deps, knit_meta)
}


# resolve html dependencies (inclusive of a format's built-in dependencies)
resolve_html_dependencies <- function(format_deps, knit_meta) {

  # list of dependencies to return (start with format_deps)
  if (!is.null(format_deps))
    dependencies <- format_deps
  else
    dependencies <- list()

  # knit_meta is a list of 'meta' attributes returned from custom knit_print
  # functions. since the 'meta' attribute could either be an html dependency or
  # a list of dependencies we recurse on lists that aren't named
  for (dep in knit_meta) {
    if (is.null(names(dep))) {
      inner_dependencies <- collect_html_dependencies(theme, dep)
      dependencies <- append(dependencies, inner_dependencies)
    }
    else if (is_html_dependency(dep)) {
      validate_html_dependency(dep)
      dependencies[[length(dependencies) + 1]] <- dep
    }
  }

  #
  # TODO: there can still be duplicate library names with different versions,
  # pick one and then merge their stylesheet, script, and head entries
  #

  # return the dependencies with duplicates removed
  unique(dependencies)
}

# create and validate an html_dependency from the passed arguments
html_dependency <- function(name,
                            version,
                            path,
                            script = NULL,
                            stylesheet = NULL,
                            head = NULL) {

  dep <- structure(list(name = name,
                        version = version,
                        path = path,
                        script = script,
                        stylesheet = stylesheet,
                        head = head),
                   class = "html_dependency")

  validate_html_dependency(dep)
}

# create an html dependency for our embedded jquery
jquery_dependency <- function()  {
  html_dependency(name = "jquery",
                  version = "1.11.0",
                  path = rmarkdown_system_file("rmd/h/jquery-1.11.0"),
                  script = "jquery.min.js")
}

# create an html dependency for our embedded bootstrap
bootstrap_dependency <- function(theme) {
  html_dependency(name = "bootstrap",
                  version = "2.3.2",
                  path = rmarkdown_system_file("rmd/h/bootstrap-2.3.2"),
                  script = "js/bootstrap.min.js",
                  stylesheet = c(paste("css/", theme, ".min.css", sep=""),
                                 "css/bootstrap-responsive.min.css"))
}

is_html_dependency <- function(list) {
  inherits(list, "html_dependency")
}

# validate that the passed list is a correctly formed html_dependency
validate_html_dependency <- function(list) {

  # ensure it's the right class
  if (!is_html_dependency(list))
    stop("passed object is not of class html_dependency", call. = FALSE)

  # validate required fields
  if (is.null(list$name))
    stop("name for html_dependency not provided", call. = FALSE)
  if (is.null(list$version))
    stop("version for html_dependency not provided", call. = FALSE)
  if (is.null(list$path))
    stop("path for html_dependency not provided", call. = FALSE)
  if (!file.exists(list$path))
    stop("path for html_dependency not found: ", list$path, call. = FALSE)

  # validate that other fields are known
  fields <- names(list)
  invalid_fields <- fields[! fields %in%
                             c("name", "version", "path", "script", "stylesheet", "head")]
  if (length(invalid_fields) > 0) {
    stop("unrecoginzed fields specified in html_dependency: ",
         paste(invalid_fields, sep = ", "), call. = FALSE)
  }

  list
}



