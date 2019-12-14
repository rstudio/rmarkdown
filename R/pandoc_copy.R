#' Copy a Pandoc's data file
#'
#' @param data The name of Pandoc's data file (e.g., "reference.docx")
#' @param output The name of the output file. If \code{NULL} (default), the name
#' will be same as the \code{data} argument.
#'
#' @return The name of the output file.
#'
#' @export
pandoc_copy_data <- function(data, output = data) {
  system(paste(
    quoted(pandoc()), "-o", output, "--print-default-data-file", data,
    collapse = " "
  ))
  output
}

#' Copy a template file for Pandoc
#'
#'
#'
#' @param format A format of the template file as a character (e.g.,
#' \code{html}) or as a result of a formatting function (e.g., \code{html_document()}).
#' @param output The name of the output file. If using \code{NULL} then the
#' output filename will be based on the \code{format} argument.
#' @param ... Arguments passed to \code{file.copy()} when \format is a result of
#' a formatting function. Otherwise, they are ignored.
#'
#' @return The name of the output file.
#'
#' @export
pandoc_copy_template <- function(format, output = NULL, ...) {
  UseMethod("pandoc_copy_template")
}

#' @export
pandoc_copy_template.character <- function(format, output = NULL, ...) {
  if (is.null(output)) output <- paste0("template.", format)
  system(paste(
    quoted(pandoc()), "-o", output, "--print-default-template", format,
    collapse = " "
  ))
  output
}

#' @export
pandoc_copy_template.rmarkdown_output_format <- function(format, output = NULL, ...) {
  args <- format$pandoc$args
  template <- args[which(args == "--template") + 1L]
  if (is.null(output)) output <- sub(".*[\\/]", "", template)
  file.copy(template, output, ...)
  output
}
