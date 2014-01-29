
#' @export
render <- function(input,
                   output = NULL,
                   output.file = NULL,
                   clean = FALSE,
                   envir = parent.frame(),
                   quiet = FALSE,
                   encoding = getOption("encoding")) {

  # check for required version of pandoc
  required_pandoc <- "1.12.3"
  if (!pandoc::available(required_pandoc)) {
    stop("pandoc version ", required_pandoc, " or higher ",
         "is required and was not found.", call. = FALSE)
  }

  # setup a cleanup function for intermediate files
  intermediates <- c()
  on.exit(sapply(intermediates,
                 function(f) unlink(f, recursive = TRUE)),
          add = TRUE)

  # execute within the input file's directory
  oldwd <- setwd(dirname(tools::file_path_as_absolute(input)))
  on.exit(setwd(oldwd), add = TRUE)

  # reset the name of the input file to be relative
  input <- basename(input)

  # read the input file
  input_lines <- readLines(input, warn = FALSE, encoding = encoding)

  # read the output format from YAML if necessary
  if (is.null(output))
    output <- output_format_from_yaml(input_lines)

  # automatically create an output file name if necessary
  if (is.null(output.file)) {

    # compute output file
    output.file <- pandoc_output_file(input, output$to)

    # if the user wants to keep the input directory clean then make
    # sure the output file goes into a temporary directory
    if (clean) {
      rmarkdown_dir <- file.path(tempdir(), "rmarkdown")
      if (!file.exists(rmarkdown_dir))
        dir.create(rmarkdown_dir)
      output.file <- file.path(rmarkdown_dir, output.file)
    }
  }

  # call any filter that's been specified
  if (!is.null(output$filter))
    output <- output$filter(output, input_lines)

  # knit if necessary
  if (tolower(tools::file_ext(input)) %in% c("rmd", "rmarkdown")) {

    # default rendering and chunk options
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)
    figures_dir <- paste("figure-", output$to, "/", sep = "")
    knitr::opts_chunk$set(fig.path=figures_dir)

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

    # clean if requested
    if (clean)
      intermediates <- c(intermediates, input, figures_dir)
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

    # always cleanup the utf8 version
    intermediates <- c(intermediates, input)
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
                  output.file,
                  TRUE,
                  output$pandoc,
                  !quiet)

  # return the full path to the output file
  invisible(tools::file_path_as_absolute(output.file))
}

