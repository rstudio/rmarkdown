#' Render R Markdown
#'
#' Render the input file to the specified output format using pandoc. If the
#' input requires knitting then \code{\link[knitr:knit]{knit}} is called prior
#' to pandoc.
#'
#' @param input Input file (Rmd or plain markdown)
#' @param output R Markdown output format to convert to (see
#'   \code{\link{output_format}}).
#' @param output.file Output file (if not specified then a default based on the
#'   specified output format is chosen)
#' @param envir The environment in which the code chunks are to be evaluated
#'   during knitting (can use \code{\link{new.env}()} to guarantee an empty new
#'   environment)
#' @param quiet \code{TRUE} to supress printing of the pandoc command line
#' @param encoding the encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
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
#' @seealso \link[knitr:knit]{knit}, \link{output_format},
#'   \href{http://johnmacfarlane.net/pandoc}{pandoc}
#'
#' @export
render <- function(input,
                   output,
                   output.file = NULL,
                   envir = parent.frame(),
                   quiet = FALSE,
                   encoding = getOption("encoding")) {

  # check for required version of pandoc
  required_pandoc <- "1.12.3"
  if (!pandoc::available(required_pandoc)) {
    stop("pandoc version ", required_pandoc, " or higher ",
         "is required and was not found.", call. = FALSE)
  }

  # execute within the input file's directory
  oldwd <- setwd(dirname(tools::file_path_as_absolute(input)))
  on.exit(setwd(oldwd), add = TRUE)

  # reset the name of the input file to be relative
  input <- basename(input)

  # automatically create an output file name if necessary
  if (is.null(output.file))
    output.file <- pandoc_output_file(input, output$to)

  # call any filter that's been specified
  if (!is.null(output$filter)) {
    input_lines <- readLines(input, warn = FALSE, encoding = encoding)
    output <- output$filter(output, input_lines)
  }

  # knit if necessary
  if (tolower(tools::file_ext(input)) %in% c("rmd", "rmarkdown")) {

    # default rendering and chunk options
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)
    knitr::opts_chunk$set(fig.path=paste("figure-", output$to, "/", sep = ""))

    # merge user options and hooks
    if (!is.null(output$knitr)) {
      knitr::opts_knit$set(as.list(output$knitr$opts_knit))
      knitr::opts_chunk$set(as.list(output$knitr$opts_chunk))
      knitr::knit_hooks$set(as.list(output$knitr$knit_hooks))
    }

    # perform the knit
    input <- knitr::knit(input,
                         envir = envir,
                         quiet = quiet,
                         encoding = encoding)
  }

  # if the encoding isn't UTF-8 then write a UTF-8 version
  if (!identical(encoding, "UTF-8")) {
    if (identical(encoding, "native.enc"))
      encoding <- ""
    input_text <- readLines(input, warn = FALSE, encoding = encoding)
    input_text <- iconv(input_text, from = encoding, to = "UTF-8")
    input <- paste(tools::file_path_sans_ext(input),
                   ".utf8.",
                   tools::file_ext(input),
                   sep = "")
    writeLines(input_text, input, useBytes = TRUE)
    input_abs <- tools::file_path_as_absolute(input)
    on.exit(file.remove(input_abs), add = TRUE)
  }

  # define markdown flavor as base pandoc markdown plus some extensions
  # for backward compatibility with github flavored markdown
  rmd <- paste("markdown",
               "+autolink_bare_uris",
               "+ascii_identifiers",
               "+tex_math_single_backslash",
               sep = "")

  # run the conversion
  pandoc::convert(input,
                  output$to,
                  rmd,
                  output.file, TRUE,
                  output$pandoc,
                  !quiet)

  # return the full path to the output file
  invisible(tools::file_path_as_absolute(output.file))
}

