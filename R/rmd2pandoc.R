#' Knit and convert an R Markdown document
#'
#' Knit the specified R Markdown input file and convert it to final output using
#' pandoc.
#'
#' @param input Input file
#' @param to Pandoc format to convert to
#' @param options Character vector of command line options to pass to pandoc.
#' @param output Output file (if not specified then a default based on the
#'   specified \code{to} format is chosen)
#' @param envir The environment in which the code chunks are to be evaluated
#'   (can use \code{\link{new.env}()} to guarantee an empty new environment)
#' @param quiet \code{TRUE} to supress printing of the pandoc command line
#' @param encoding the encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @details Typically one of the \code{\link{knitrRender}} functions is called
#'   prior to calling \code{knit2pandoc} to optimize knitr rendering for the
#'   intended output format.
#'
#' @export
rmd2pandoc <- function(input,
                       to,
                       options = NULL,
                       output = NULL,
                       envir = parent.frame(),
                       quiet = FALSE,
                       encoding = getOption("encoding")) {

  # execute within the input file's directory
  oldwd <- setwd(dirname(tools::file_path_as_absolute(input)))
  on.exit(setwd(oldwd), add = TRUE)

  # knit
  input <- knitr::knit(input, envir = envir, quiet = quiet, encoding = encoding)

  # re-write the file in UTF-8 for passing to pandoc
  if (identical(encoding, "native.enc"))
    encoding <- ""
  inputText <- readLines(input, encoding = encoding)
  inputText <- iconv(inputText, from = encoding, to = "UTF-8")
  writeLines(inputText, input, useBytes = TRUE)

  # get the path to pandoc
  pandoc <- pandocPath()

  # build pandoc args
  args <- c(input)

  # pandoc: convert to format
  args <- c(args, "--to", to)

  # pandoc: support full syntax of pandoc markdown with some additional
  # features for backward compatibility with github flavored markdown
  args <- c(args, "--from",
            paste0("markdown",
                   "+autolink_bare_uris",
                   "+ascii_identifiers",
                   "+tex_math_single_backslash"))

  # citeproc filter
  args <- c(args, "--filter", file.path(dirname(pandoc), "pandoc-citeproc"))

  # pandoc: additional command line options
  args <- c(args, options)

  # pandoc: output file
  if (is.null(output))
    output <- pandocOutputFile(input, to)
  args <- c(args, "--output", output)


  # show pandoc command line if requested
  if (!quiet) {
    cat(paste0(pandoc, " "))
    cat(paste(args, collapse=" "))
    cat("\n")
  }

  # run the conversion
  result <- pandocExec(pandoc, args)

  # if we succeeded then return the full path of the output file,
  # else throw an error
  if (result == 0)
    invisible(tools::file_path_as_absolute(output))
  else
    stop("pandoc document conversion failed", call. = FALSE)
}



pandocTemplate <- function(file) {
  system.file(file.path("templates", file), package = "rmarkdown")
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
  command <- paste(shQuote(pandoc), paste(shQuote(args), collapse = " "))
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
  if (version < pandocRequiredVersion()) {
    stop("The rmarkdown package requires pandoc version ",
         as.character(pandocRequiredVersion()), " ",
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

pandocRequiredVersion <- function() {
  numeric_version("1.12.3")
}
