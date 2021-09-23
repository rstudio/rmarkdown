#' Create a new document based on a template
#'
#' Create (and optionally edit) a draft of an R Markdown document based on a
#' template.
#'
#' The \code{draft} function creates new R Markdown documents based on
#' templates that are either located on the filesystem or within an R package.
#' The template and its supporting files will be copied to the location
#' specified by \code{file}.
#' @param file File name for the draft
#' @param template Template to use as the basis for the draft. This is either
#'   the full path to a template directory or the name of a template directory
#'   within the \code{rmarkdown/templates} directory of a package.
#' @param package (Optional) Name of package where the template is located.
#' @param create_dir \code{TRUE} to create a new directory for the document
#'   (the "default" setting leaves this behavior up to the creator of the
#'   template).
#' @param edit \code{TRUE} to edit the template immediately
#' @return The file name of the new document (invisibly).
#' @note An R Markdown template consists of a directory that contains a
#'   description of the template, a skeleton Rmd file used as the basis for new
#'   documents, and optionally additional supporting files that are provided
#'   along with the skeleton (e.g. a logo graphic).
#'
#'   If the template directory is contained within a package then it should be
#'   located at \code{inst/rmarkdown/templates}. For example, a package named
#'   \pkg{pubtools} that wanted to provide a template named
#'   \code{quarterly_report} would need to provide the following files within
#'   the \code{pubtools/inst/rmarkdown/templates} directory:
#'
#'   \code{quarterly_report/template.yaml} \cr
#'   \code{quarterly_report/skeleton/skeleton.Rmd} \cr
#'
#'   The \code{template.yaml} file should include a \code{name} field. If you
#'   want to ensure that a new directory is always created for a given template,
#'   then you can add the \code{create_dir} field to the \code{template.yaml}
#'   file. For example:
#'
#'   \code{create_dir: true} \cr
#'
#'   The \code{skeleton/skeleton.Rmd} file should include the initial contents
#'   you want for files created from this template. Additional files can be
#'   added to the \code{skeleton} directory, for example:
#'
#'   \code{skeleton/logo.png} \cr
#'
#'   These files will automatically be copied to the directory containing the
#'   new R Markdown draft.
#' @examples
#' \dontrun{
#' rmarkdown::draft("Q4Report.Rmd",
#'                  template="/opt/rmd/templates/quarterly_report")
#'
#' rmarkdown::draft("Q4Report.Rmd",
#'                  template="quarterly_report", package="pubtools")
#' }
#' @export
draft <- function(file,
                  template,
                  package = NULL,
                  create_dir = "default",
                  edit = TRUE) {

  # resolve package file
  if (!is.null(package)) {
    template_path = pkg_file("rmarkdown", "templates", template,
                                package = package)
    if (!nzchar(template_path)) {
      stop("The template '", template, "' was not found in the ",
           package, " package")
    }
  } else {
    template_path <- template
  }

  # read the template.yaml and confirm it has the right fields
  template_yaml <- file.path(template_path, "template.yaml")
  if (!file.exists(template_yaml)) {
    stop("No template.yaml file found for template '", template, "'")
  }

  template_meta <- yaml_load_file(template_yaml)
  if (is.null(template_meta$name) || is.null(template_meta$description)) {
    stop("template.yaml must contain name and description fields")
  }

  # see if this template is asking to create a new directory
  if (identical(create_dir, "default"))
    create_dir <- isTRUE(template_meta$create_dir)

  # create a new directory if requested
  if (create_dir) {

    # remove .Rmd extension if necessary
    file <- xfun::sans_ext(file)

    # create dir (new dir only)
    if (dir_exists(file))
      stop("The directory '", file, "' already exists.")
    dir.create(file)

    # reconstitute the file path
    file <- file.path(file, basename(file))
  }

  # Ensure we have an Rmd extension
  if (!identical(tolower(xfun::file_ext(file)), "rmd"))
    file <- paste(file, ".Rmd", sep = "")

  # Ensure the file doesn't already exist
  if (file.exists(file))
    stop("The file '", file, "' already exists.")

  # copy all of the files in the skeleton directory
  skeleton_files <- list.files(file.path(template_path, "skeleton"),
                               full.names = TRUE)
  to <- dirname(file)
  for (f in skeleton_files) {
    if (file.exists(file.path(to, basename(f))))
      stop("The file '", basename(f), "' already exists")
    file.copy(from = f, to = to, overwrite = FALSE, recursive = TRUE)
  }

  # rename the core template file
  file.rename(file.path(dirname(file), "skeleton.Rmd"), file)

  # invoke the editor if requested
  if (edit)
    utils::file.edit(normalizePath(file))

  # return the name of the file created
  invisible(file)
}

# List the template directories that are available for consumption.
list_template_dirs <- function() {

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
