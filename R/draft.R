
#' Create a new document based on a template
#'
#' Create (and optionally edit) a draft of an R Markdown document based on a
#' template.
#'
#' @param file File name for the draft
#' @param template Template to use as the basis for the draft. This is either
#'   the full path to a template directory or a reference to a template stored
#'   within an R package (e.g. \code{pkgname::my_template}).
#' @param edit \code{TRUE} to edit the template immediately
#'
#' @return The file name of the new document (invisibly)
#'
#' @details The \code{draft} function creates new R Markdown documents based on
#'   templates that are either located on the filesystem or within an R package.
#'   The template and it's supporting files will be copied to the location
#'   specified by \code{file}.
#'
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
#'   \code{quarterly_report/skeleton/template.Rmd} \cr
#'
#'   The \code{template.yaml} file should include the two fields \code{name} and
#'   \code{description}. If you want to ensure that a new directory is always
#'   created for a given template, then you can add the \code{create_dir} field
#'   to the \code{template.yaml} file. For example:
#'
#'   \code{create_dir: true} \cr
#'
#'   The \code{skeleton/template.Rmd} file should include the initial contents
#'   you want for files created from this template. Additional files can be
#'   added to the \code{skeleton} directory, for example:
#'
#'   \code{skeleton/logo.png} \cr
#'
#'   These files will automatically be copied to the directory containing the
#'   new R Markdown draft.
#'
#'
#' @examples
#' \dontrun{
#'
#' rmarkdown::draft("Q4Report.Rmd", template="pubtools::quarterly_report")
#' rmarkdown::draft("Q4Report.Rmd", template="/templates/quarterly_report")
#' }
#' @export
draft <- function(file, template, edit = TRUE) {

  # see if there is a package name embedded in the template
  names <- strsplit(template, split = "::", fixed = TRUE)[[1]]
  if (length(names) == 2) {
    package <- names[[1]]
    template <- names[[2]]
    template_path = system.file("rmarkdown", "templates", template,
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
  template_meta <- yaml::yaml.load_file(template_yaml)
  if (is.null(template_meta$name) || is.null(template_meta$description)) {
    stop("template.yaml must contain name and description fields")
  }

  # see if this template is asking to create a new directory
  create_dir <- isTRUE(template_meta$create_dir)

  # create a new directory if requested
  if (create_dir) {

    # remove .Rmd extension if necessary
    file <- tools::file_path_sans_ext(file)

    # create dir (new dir only)
    if (file.exists(file))
      stop("The directory '", file, "' already exists.")
    dir.create(file)

    # reconstitute the file path
    file <- file.path(file, basename(file))
  }

  # Ensure we have an Rmd extension
  if (!identical(tolower(tools::file_ext(file)), "rmd"))
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
  file.rename(file.path(dirname(file), "template.Rmd"), file)

  # invoke the editor if requested
  if (edit)
    file.edit(normalizePath(file))

  # return the name of the file created
  invisible(file)
}
