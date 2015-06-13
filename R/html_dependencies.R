#' @import htmltools
NULL

# create an html dependency for our embedded jquery
html_dependency_jquery <- function()  {
  htmlDependency(name = "jquery",
                 version = "1.11.0",
                 src = rmarkdown_system_file("rmd/h/jquery-1.11.0"),
                 script = "jquery.min.js")
}

# create an html dependency for our embedded bootstrap
html_dependency_bootstrap <- function(theme) {
  
  # inline bootstrap theme b/c pandoc 1.14 base64 encoding 
  # somehow borks up reading bootstrap.min.css. it also borks
  # up the handling of embedded fonts (as bootstrap themes that
  # reference embedded fonts actually end up with errors). note
  # that is implies that themes won't get their embedded fonts
  # but this is superior to having rendering fail with an error!
  #
  # see pandoc bug filed here: https://github.com/jgm/pandoc/issues/2224
  
  if (pandoc_available("1.14")) {
    theme_css <- rmarkdown_system_file(paste0("rmd/h/bootstrap-3.3.1/css/", 
                                              theme, ".min.css"))
    theme_style_tag <- paste(c('<style type="text/css">',
                             readLines(theme_css, warn = FALSE),
                             '</style>'),
                             sep = "\n")
    
    htmlDependency(name = "bootstrap",
                   version = "3.3.1",
                   rmarkdown_system_file("rmd/h/bootstrap-3.3.1"),
                   meta = list(viewport = "width=device-width, initial-scale=1"),
                   script = c(
                     "js/bootstrap.min.js",
                     # These shims are necessary for IE 8 compatibility
                     "shim/html5shiv.min.js",
                     "shim/respond.min.js"
                   ),
                   head = theme_style_tag
    )
  } else {
    htmlDependency(name = "bootstrap",
                   version = "3.3.1",
                   rmarkdown_system_file("rmd/h/bootstrap-3.3.1"),
                   meta = list(viewport = "width=device-width, initial-scale=1"),
                   script = c(
                     "js/bootstrap.min.js",
                     # These shims are necessary for IE 8 compatibility
                     "shim/html5shiv.min.js",
                     "shim/respond.min.js"
                   ),
                   stylesheet = paste("css/", theme, ".min.css", sep="")
    )
  }
}

# flattens an arbitrarily nested list and returns all of the html_dependency
# objects it contains
flatten_html_dependencies <- function(knit_meta) {

  all_dependencies <- list()

  # knit_meta is a list of 'meta' attributes returned from custom knit_print
  # functions. since the 'meta' attribute could either be an html dependency or
  # a list of dependencies we recurse on lists that aren't named
  for (dep in knit_meta) {
    if (is.null(names(dep)) && is.list(dep)) {
      inner_dependencies <- flatten_html_dependencies(dep)
      all_dependencies <- append(all_dependencies, inner_dependencies)
    }
    else if (is_html_dependency(dep)) {
      all_dependencies[[length(all_dependencies) + 1]] <- dep
    }
  }

  all_dependencies
}

# consolidate dependencies (use latest versions and remove duplicates). this
# routine is the default implementation for version dependency resolution;
# formats may specify their own.
html_dependency_resolver <- function(all_dependencies) {

  dependencies <- htmltools::resolveDependencies(all_dependencies)

  # validate each surviving dependency
  lapply(dependencies, validate_html_dependency)

  # return the consolidated dependencies
  dependencies
}

html_reference_path <- function(path, lib_dir, output_dir) {
  # write the full OS-specific path if no library
  if (is.null(lib_dir))
    pandoc_path_arg(path)
  else
    relative_to(output_dir, path)
}

# return the html dependencies as an HTML string suitable for inclusion
# in the head of a document
html_dependencies_as_string <- function(dependencies, lib_dir, output_dir) {

  if (!is.null(lib_dir)) {
    dependencies <- lapply(dependencies, copyDependencyToDir, lib_dir)
    dependencies <- lapply(dependencies, makeDependencyRelative, output_dir)
  }
  return(renderDependencies(dependencies, "file", encodeFunc = identity,
    hrefFilter = function(path) {
      html_reference_path(path, lib_dir, output_dir)
    })
  )
}

# check class of passed list for 'html_dependency'
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
  if (is.null(list$src$file))
    stop("path for html_dependency not provided", call. = FALSE)
  if (!file.exists(list$src$file))
    stop("path for html_dependency not found: ", list$src$file, call. = FALSE)

  list
}

# check if the passed knit_meta has any html dependencies
has_html_dependencies <- function(knit_meta) {

  if (inherits(knit_meta, "html_dependency"))
    return(TRUE)

  else if (is.list(knit_meta)) {
    for (dep in knit_meta) {
      if (is.null(names(dep))) {
        if (has_html_dependencies(dep))
          return(TRUE)
      } else {
        if (inherits(dep, "html_dependency"))
          return(TRUE)
      }
    }

    FALSE
  } else {
    FALSE
  }
}
