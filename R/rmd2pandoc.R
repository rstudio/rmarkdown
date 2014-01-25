#' Convert R Markdown Using Pandoc
#'
#' Convert the input file using pandoc. If the input requires knitting then
#' \code{\link[knitr:knit]{knit}} is called prior to pandoc.
#'
#' @param input Input file (Rmd or plain markdown)
#' @param to Pandoc format to convert to
#' @param options Character vector of command line options to pass to pandoc.
#' @param output Output file (if not specified then a default based on the
#'   specified \code{to} format is chosen)
#' @param envir The environment in which the code chunks are to be evaluated
#'   during knitting (can use \code{\link{new.env}()} to guarantee an empty new
#'   environment)
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
#' @section R Markdown: R Markdown supports all of the base pandoc markdown
#'   features as well as some optional features for compatibility with GitHub
#'   Flavored Markdown (which previous versions of R Markdown were based on).
#'
#'   For more on pandoc markdown see the
#'   \href{http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html}{pandoc
#'    markdown specification}. Optional pandoc markdown features enabled include
#'   \code{autolink_bare_uris}, \code{ascii_identifiers}, and
#'   \code{tex_math_single_backslash}.
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

  # define markdown flavor as base pandoc markdown plus some extensions
  # for backward compatibility with github flavored markdown
  rmdFormat <- paste0("markdown",
                      "+autolink_bare_uris",
                      "+ascii_identifiers",
                      "+tex_math_single_backslash")

  # automatically create an output file name if necessary
  if (is.null(output))
    output <- pandocOutputFile(input, to)

  # knit if necessary
  if (knitRequired(input)) {
    input <- knitr::knit(input,
                         envir = envir,
                         quiet = quiet,
                         encoding = encoding)
  }

  # if the encoding isn't UTF-8 then write a UTF-8 version
  if (!identical(encoding, "UTF-8")) {
    if (identical(encoding, "native.enc"))
      encoding <- ""
    inputText <- readLines(input, encoding = encoding)
    inputText <- iconv(inputText, from = encoding, to = "UTF-8")
    input <- paste0(tools::file_path_sans_ext(input),
                    ".utf8.",
                    tools::file_ext(input))
    on.exit(unlink(input), add = TRUE)
    writeLines(inputText, input, useBytes = TRUE)
  }

  # run the conversion
  pandoc::convert(input, to, rmdFormat, output, TRUE, options, !quiet)

  # return the full path to the output file
  tools::file_path_as_absolute(output)
}



