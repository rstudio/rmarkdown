# List the template directories that are available for consumption.
# This function is only used by RStudio IDE in version before 1.1.74
# https://github.com/rstudio/rstudio/commit/01b26d1afd8e403fe1d026cba9264bf983a86173
# TODO: Remove this function
list_template_dirs <- function() {
  warning(
    "`list_template_dirs()` is deprecated and will be defunct in future version",
    call. = FALSE, immediate. = TRUE
  )
  # check each installed package for templates
  packages <- row.names(utils::installed.packages())
  for (pkg in packages) {

    # check to see if the package includes a template folder
    template_folder <- system.file("rmarkdown", "templates", package = pkg)
    if (dir_exists(template_folder)) {

      # it does; list each template directory within the template folder
      template_dirs <- list.dirs(path = template_folder, recursive = FALSE)
      for (dir in template_dirs) {
        cat(pkg, "|", dir, "\n", sep = "")
      }
    }
  }
}
