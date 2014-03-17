
#' Create a new document based on a template
#'
#' Create (and optionally edit) a draft of an R Markdown document based on a
#' template.
#'
#' @param file File name for the draft
#' @param template Template to use as the basis for the draft. This is either
#'   the full path to a template directory or the name of a template directory
#'   within the \code{rmarkdown/templates} directory of a package.
#' @param package (Optional) Name of package where the template is located.
#' @param edit \code{TRUE} to edit the template immediately
#'
#' @return The file name of the new document (invisibly)
#'
#' @details The \code{draft} function creates new R Markdown documents based
#'   on templates that are either located on the filesystem or within an R
#'   package. A new directory bearing the same name as the R Markdown file is
#'   created then the template and any supporting files are copied into the
#'   directory.
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
#'   \code{description}. The \code{skeleton/template.Rmd} file should include
#'   the initial contents you want for files created from this template.
#'   Additional files can be added to the \code{skeleton} directory, for
#'   example:
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
#' rmarkdown::draft("Q4Report.Rmd",
#'                  template="/opt/rmd/templates/quarterly_report")
#'
#' rmarkdown::draft("Q4Report.Rmd",
#'                  template="quarterly_report", package="pubtools")
#' }
#' @export
draft <- function(file, template, package = NULL, edit = TRUE) {

  # remove .Rmd extension and create a new directory with that name
  file <- tools::file_path_sans_ext(file)
  if (file.exists(file))
    stop("The directory '", file, "' already exists.")
  dir.create(file)

  # reconstitute the file path within the directory and w/ the .Rmd extension
  file <- file.path(file, paste(basename(file), "Rmd", sep="."))

  # resolve package file
  if (!is.null(package)) {
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

  # copy all of the files in the skeleton directory
  skeleton_files <- list.files(file.path(template_path, "skeleton"),
                               full.names = TRUE)
  file.copy(from = skeleton_files,
            to = dirname(file),
            recursive = TRUE)

  # rename the core template file
  file.rename(file.path(dirname(file), "template.Rmd"), file)

  # invoke the editor if requested
  if (edit)
    file.edit(normalizePath(file))

  # return the name of the file created
  invisible(file)
}
