
# create and validate an html_dependency from the passed arguments
html_dependency <- function(name,
                            version,
                            path,
                            meta = NULL,
                            script = NULL,
                            stylesheet = NULL,
                            head = NULL,
                            external = NULL) {

  dep <- structure(list(name = name,
                        version = version,
                        path = path,
                        meta = meta,
                        script = script,
                        stylesheet = stylesheet,
                        head = head,
                        external = external),
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

# copy the library directory of a dependency and return the path to
# the directory which was copied to
html_dependency_copy_lib <- function(dependency, lib_dir) {

  # auto-create lib_dir
  if (!file.exists(lib_dir))
    dir.create(lib_dir)

  # target directory is based on the dirname of the path
  target_dir <- file.path(lib_dir, basename(dependency$path))

  # copy the directory
  file.copy(from = dependency$path,
            to = lib_dir,
            overwrite = TRUE,
            recursive = TRUE)

  # return the target dir
  target_dir
}

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
      validate_html_dependency(dep)
      all_dependencies[[length(all_dependencies) + 1]] <- dep
    }
  }

  all_dependencies
}

# consolidate dependencies (use latest versions and remove duplicates)
html_dependency_resolver <- function(all_dependencies) {

  dependencies <- list()
  for (dep in unique(all_dependencies)) {
    # if we already have a library of this name then re-use it
    if (!is.null(dependencies[[dep$name]])) {
      version <- dependencies[[dep$name]]$version
      is_newer_version <-
        numeric_version(dep$version) > numeric_version(version)
      is_older_version <-
        numeric_version(version) > numeric_version(dep$version)

      # test for sanity: we can never resolve an external version conflict
      if (isTRUE(dep$external) && isTRUE(dependencies[[dep$name]]$external) &&
          !identical(dep$version, version)) {
        stop(dep$name, " specified with conflicting external versions ",
             dep$version, ", ", version)
      }

      # if the dependency was externally satisfied with a version earlier than
      # the one requested, emit a warning
      if ((is_newer_version && isTRUE(dependencies[[dep$name]]$external)) ||
          (is_older_version && isTRUE(dep$external))) {
        warning(dep$name, " replaced by externally supplied ",
                "older version (", dep$version, ", ", version, ")")
      }

      # an incoming external dependency always wins over an existing dependency;
      # an incoming internal dependency wins if its version is newer than an
      # existing internal dependency
      if ((is_newer_version && !isTRUE(dependencies[[dep$name]]$external)) ||
          isTRUE(dep$external)) {
        dependencies[[dep$name]]$version <- dep$version
        dependencies[[dep$name]]$path <- dep$path
        dependencies[[dep$name]]$script <- dep$script
        dependencies[[dep$name]]$stylesheet <- dep$stylesheet
        dependencies[[dep$name]]$external <- dep$external
      }

      # consolidate other fields
      dependencies[[dep$name]]$meta <-
        merge_lists(dependencies[[dep$name]]$meta, dep$meta)
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

# return the html dependencies as an HTML string suitable for inclusion
# in the head of a document
html_dependencies_as_string <- function(dependencies, lib_dir) {

  dependencies_html <- c()

  for (dep in dependencies) {

    # if the dependency is externally satisfied, don't emit it
    if (isTRUE(dep$external))
      next

    # copy library files if necessary
    if (!is.null(lib_dir)) {
      dep$path <- html_dependency_copy_lib(dep, lib_dir)
    }

    # add meta content
    for (name in names(dep$meta)) {
      dependencies_html <- c(dependencies_html,
        paste("<meta name=\"", name, "\" content=\"", dep$meta[[name]], "\" />", sep = ""))
    }

    # add stylesheets
    for (stylesheet in dep$stylesheet) {
      stylesheet <- pandoc_path_arg(file.path(dep$path, stylesheet))
      dependencies_html <- c(dependencies_html,
        paste("<link href=\"", stylesheet, "\" rel=\"stylesheet\" />", sep = ""))
    }

    # add scripts
    for (script in dep$script) {
      script <- pandoc_path_arg(file.path(dep$path, script))
      dependencies_html <- c(dependencies_html,
        paste("<script src=\"", script, "\"></script>", sep = ""))
    }

    # add raw head content
    dependencies_html <- c(dependencies_html, dep$head)
  }

  dependencies_html
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
  if (is.null(list$path))
    stop("path for html_dependency not provided", call. = FALSE)
  if (!file.exists(list$path))
    stop("path for html_dependency not found: ", list$path, call. = FALSE)

  # validate that other fields are known
  fields <- names(list)
  invalid_fields <- fields[! fields %in%
                             c("name", "version", "path", "meta",
                               "script", "stylesheet", "head", "external")]
  if (length(invalid_fields) > 0) {
    stop("unrecoginzed fields specified in html_dependency: ",
         paste(invalid_fields, sep = ", "), call. = FALSE)
  }

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
