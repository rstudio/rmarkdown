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

  # check for required version of pandoc
  requiredPandoc <- "1.12.3"
  if (!pandoc::available(requiredPandoc)) {
    stop("pandoc version ", requiredPandoc, " or higher ",
         "is required and was not found.", call. = FALSE)
  }

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

  # Rmd format - support full syntax of pandoc markdown with some additional
  # features for backward compatibility with github flavored markdown
  from <- paste0("markdown",
                 "+autolink_bare_uris",
                 "+ascii_identifiers",
                 "+tex_math_single_backslash")

  # run the conversion
  pandoc::convert(input, from, to, TRUE, options, output, !quiet)
}

pandocTemplate <- function(file) {
  system.file(file.path("templates", file), package = "rmarkdown")
}
