
# create and validate an html_dependency from the passed arguments
html_dependency <- function(name,
                            version,
                            path,
                            meta = NULL,
                            script = NULL,
                            stylesheet = NULL,
                            head = NULL) {

  dep <- structure(list(name = name,
                        version = version,
                        path = path,
                        meta = meta,
                        script = script,
                        stylesheet = stylesheet,
                        head = head),
                   class = "html_dependency")

  validate_html_dependency(dep)
}

# create an html dependency for our embedded jquery
html_dependency_jquery <- function()  {
  html_dependency(name = "jquery",
                  version = "1.11.0",
                  path = rmarkdown_system_file("rmd/h/jquery-1.11.0"),
                  script = "jquery.min.js")
}

# create an html dependency for our embedded bootstrap
html_dependency_bootstrap <- function(theme) {
  html_dependency(name = "bootstrap",
                  version = "2.3.2",
                  path = rmarkdown_system_file("rmd/h/bootstrap-2.3.2"),
                  meta = list(viewport = "width=device-width, initial-scale=1.0"),
                  script = "js/bootstrap.min.js",
                  stylesheet = c(paste("css/", theme, ".min.css", sep=""),
                                 "css/bootstrap-responsive.min.css"))
}


# resolve html dependencies (inclusive of a format's built-in dependencies)
html_dependencies_for_document <- function(format_deps, knit_meta) {

  # list of dependencies to return (start with format_deps)
  if (!is.null(format_deps))
    all_dependencies <- format_deps
  else
    all_dependencies <- list()

  # knit_meta is a list of 'meta' attributes returned from custom knit_print
  # functions. since the 'meta' attribute could either be an html dependency or
  # a list of dependencies we recurse on lists that aren't named
  for (dep in knit_meta) {
    if (is.null(names(dep))) {
      inner_dependencies <- collect_html_dependencies(theme, dep)
      all_dependencies <- append(all_dependencies, inner_dependencies)
    }
    else if (is_html_dependency(dep)) {
      validate_html_dependency(dep)
      all_dependencies[[length(all_dependencies) + 1]] <- dep
    }
  }

  # consolidate dependencies (use latest versions and remove duplicates)
  dependencies <- list()
  for (dep in unique(all_dependencies)) {
    # if we already have a library of this name then re-use it
    if (!is.null(dependencies[[dep$name]])) {
      # if this one is newer then use it's path/version
      version <- dependencies[[dep$name]]$version
      if (numeric_version(dep$version) > numeric_version(version)) {
        dependencies[[dep$name]]$version <- dep$version
        dependencies[[dep$name]]$path <- dep$path
      }
      # consolidate other fields
      dependencies[[dep$name]]$meta <-
        merge_lists(dependencies[[dep$name]]$meta, dep$meta)
      dependencies[[dep$name]]$script <-
        unique(c(dependencies[[dep$name]]$script, dep$script))
      dependencies[[dep$name]]$stylesheet <-
        unique(c(dependencies[[dep$name]]$stylesheet, dep$stylesheet))
      dependencies[[dep$name]]$head <-
        unique(c(dependencies[[dep$name]]$head, dep$head))
      # first instance of this library, just copy over all the fields
    } else {
      dependencies[[dep$name]] <- dep
    }
  }

  # return the consolidated dependencies
  dependencies
}

# convert a set of html dependencies to the pandoc args required to include
# them (genereates html for the dependencies then injects it into the head
# using the --include-in-header argument)
html_dependencies_to_pandoc_args <- function(dependencies,
                                             self_contained,
                                             lib_dir) {

  dependencies_html <- c()

  for (dep in dependencies) {

    # copy library files if necessary
    if (!self_contained) {
      dep$path <- render_supporting_files(dep$path, lib_dir)
    }

    # add meta content
    for (name in names(dep$meta)) {
      dependencies_html <- c(dependencies_html,
        paste("<meta name=\"", name, "\" content=\"", dep$meta[[name]], "\" />", sep = ""))
    }

    # add stylesheets
    for (stylesheet in dep$stylesheet) {
      stylesheet <- file.path(dep$path, stylesheet)
      dependencies_html <- c(dependencies_html,
        paste("<link href=\"", stylesheet, "\" rel=\"stylesheet\" />", sep = ""))
    }

    # add scripts
    for (script in dep$script) {
      script <- file.path(dep$path, script)
      dependencies_html <- c(dependencies_html,
        paste("<script src=\"", script, "\"></script>", sep = ""))
    }

    # add raw head content
    dependencies_html <- c(dependencies_html, dep$head)
  }

  # write to a temp file and include it in the document
  if (length(dependencies_html) > 0) {
    deps_file <- tempfile("rmarkdown-head", fileext = ".html")
    writeLines(dependencies_html, deps_file)
    pandoc_include_args(in_header = deps_file)
  } else {
    NULL
  }
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
                             c("name", "version", "path", "meta",
                               "script", "stylesheet", "head")]
  if (length(invalid_fields) > 0) {
    stop("unrecoginzed fields specified in html_dependency: ",
         paste(invalid_fields, sep = ", "), call. = FALSE)
  }

  list
}



