
# collect all dependencies provided by the passed knit_meta (also merge in
# any dependencies implied by our document theme)
collect_html_dependencies <- function(theme, knit_meta) {

  # list of dependencies to return (start with jquery and bootstrap if
  # have a document theme defined)
  if (!is.null(theme))
    dependencies <- list(jquery_dependency(), bootstrap_dependency(theme))
  else
    dependencies <- list()


  # knit_meta is a list of 'meta' attributes returned from custom knit_print
  # functions. since the 'meta' attribute could either be an html dependency or
  # a list of dependencies we recurse on lists that aren't named
  for (dep in knit_meta) {
    if (is.null(names(dep)))
      dependencies <- append(dependencies, collect_html_dependencies(theme, dep))
    else
      dependencies[[length(dependencies) + 1]] <- html_dependency(dep)
  }

  #
  # TODO: there can still be duplicate library names with different versions,
  # pick one and then merge their stylesheet, script, and head entries
  #

  # return the dependencies with duplicates removed
  unique(dependencies)
}

# create an html dependency based on a list (typically acquired from knit_meta,
# but could also be one of the built in dependencies above).
html_dependency <- function(dep_list) {

  # validate required fields
  if (is.null(dep_list$name))
    stop("name for html dependency not provided", call. = FALSE)
  if (is.null(dep_list$version))
    stop("version for html dependency not provided", call. = FALSE)
  if (is.null(dep_list$path))
    stop("path for html dependency not provided", call. = FALSE)
  if (!file.exists(dep_list$path))
    stop("path for html dependency not found: ", dep_list$path, call. = FALSE)

  # validate that other fields are known
  fields <- names(dep_list)
  invalid_fields <- fields[! fields %in%
                             c("name", "version", "path", "script", "stylesheet", "head")]
  if (length(invalid_fields) > 0) {
    stop("unrecoginzed fields specified in html dependency: ",
         paste(invalid_fields, sep = ", "), call. = FALSE)
  }

  # return the dependency info
  structure(dep_list, class = "html_dependency")
}



# create an html dependency for our embedded jquery
jquery_dependency <- function()  {

  name = "jquery"
  version <- "1.11.0"
  path <- rmarkdown_system_file(paste("rmd/h/", name, "-", version, sep=""))

  html_dependency(list(name = name,
                       version = version,
                       path = path,
                       script = "jquery.min.js"))
}

# create an html dependency for our embedded bootstrap
bootstrap_dependency <- function(theme) {

  name <- "bootstrap"
  version <- "2.3.2"
  path <- rmarkdown_system_file(paste("rmd/h/", name, "-", version, sep=""))
  theme_css <- paste("css/", theme, ".min.css", sep="")

  html_dependency(list(name = name,
                       version = version,
                       path = path,
                       script = "js/bootstrap.min.js",
                       stylesheet = c(theme_css, "css/bootstrap-responsive.min.css")))
}

