

#' Pandoc conversion options
#'
#' Generic method to get pandoc command-line options from an object (typically
#' a list containing format-specific options).
#'
#' @param options An object that implements the \code{pandocOptions} S3 method.
#'
#' @return A character vector with pandoc command line arguments
#'
#' @export
pandocOptions <- function(options) {
  UseMethod("pandocOptions", options)
}

pandocOptions.default <- function(options) {
  options
}

#' Pandoc options for including additional content
#'
#' Options for including HTML or LaTeX content in the header or before and after
#' the document body.
#'
#' @param include.header One or more files with content to be included in the
#'   header of the document.
#' @param include.before One or more files with content to be included before
#'   the document body.
#' @param include.after One or more files with content to be included after the
#'   document body.
#'
#' @return A character vector with pandoc command line arguments
#'
#' @details Non-absolute paths for resources referenced from the
#'   \code{include.header}, \code{include.before}, and \code{include.after}
#'   parameters are resolved relative to the directory of the input document.
#'
#' @export
pandocIncludeOptions <- function(include.header = NULL,
                                 include.before = NULL,
                                 include.after = NULL) {
  structure(list(include.header = include.header,
                 include.before = include.before,
                 include.after = include.after),
            class = "pandocIncludeOptions")
}

#' @S3method pandocOptions pandocIncludeOptions
pandocOptions.pandocIncludeOptions <- function(options) {

  args <- c()

  for (header in options$include.header)
    args <- c(args, "--include-in-header", header)

  for (before in options$include.before)
    args <- c(args, "--include-before-body", before)

  for (after in options$include.after)
    args <- c(args, "--include-after-body", after)

  args
}


pandocTemplateOptions <- function(template) {
  c("--template", template,
    "--data-dir", dirname(template))
}

pandocHighlightOptions <- function(highlightOptions) {

  options <- c()

  if (is.null(highlightOptions$highlight))
    options <- c(options, "--no-highlight")
  else {
    highlight <- highlightOptions$highlight
    if (identical(highlight, "default"))
      highlight <- "pygments"
    options <- c(options, "--highlight-style", highlight)
  }
  options
}

pandocTemplate <- function(file) {
  system.file(file.path("templates", file), package = "rmarkdown")
}

pandocTableOfContentsOptions <- function(tocOptions) {

  options <- c()

  if (tocOptions$toc) {
    options <- c(options, "--table-of-contents")
    options <- c(options, "--toc-depth", tocOptions$toc.depth)
  }

  options
}



pandocOutputFile <- function(input, pandocFormat) {
  if (pandocFormat %in% c("latex", "beamer"))
    ext <- ".pdf"
  else if (pandocFormat %in% c("html", "html5", "revealjs"))
    ext <- ".html"
  else
    ext <- paste0(".", pandocFormat)
  output <- paste0(tools::file_path_sans_ext(input), ext)
  output
}

pandocExec <- function(pandoc, args, ...) {
  command <- paste(pandoc, paste(shQuote(args), collapse = " "))
  system(command, ...)
}

pandocPath <- function() {

  # check for versions of pandoc in rstudio and on the path
  rstudioPandocPath <- Sys.getenv("RSTUDIO_PANDOC")
  if (nzchar(rstudioPandocPath))
    rstudioPandocPath <- file.path(rstudioPandocPath, "pandoc")
  systemPandocPath = Sys.which("pandoc")

  # determine which one is more recent
  if (!nzchar(rstudioPandocPath) && !nzchar(systemPandocPath)) {
    stop("No version of pandoc was found on the path.", call.=FALSE)
  }
  else if (!nzchar(rstudioPandocPath))
    pandoc <- systemPandocPath
  else if (!nzchar(systemPandocPath))
    pandoc <- rstudioPandocPath
  else {
    rstudioVersion <- pandocVersion(rstudioPandocPath)
    systemVersion <- pandocVersion(systemPandocPath)
    if (rstudioVersion >= systemVersion)
      pandoc <- rstudioPandocPath
    else
      pandoc <- systemPandocPath
  }

  # verify the version
  version <- pandocVersion(pandoc)
  if (version < requiredPandocVersion()) {
    stop("The rmarkdown package requires pandoc version ",
         as.character(requiredPandocVersion()), " ",
         "or greater. You currently have version ", as.character(version), " ",
         "installed. Please update to a newer version.",
         call. = FALSE)
  }

  # return the path to pandoc
  pandoc
}

pandocVersion <- function(pandocPath) {
  versionInfo <- system(paste(shQuote(pandocPath), "--version"), intern = TRUE)
  version <- strsplit(versionInfo, "\n")[[1]][1]
  version <- strsplit(version, " ")[[1]][2]
  numeric_version(version)
}

requiredPandocVersion <- function() {
  numeric_version("1.12.3")
}





