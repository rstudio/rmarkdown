
#' @export
render <- function(input,
                   output.format = NULL,
                   output.file = NULL,
                   clean = TRUE,
                   envir = parent.frame(),
                   quiet = FALSE,
                   encoding = getOption("encoding")) {

  # check for required version of pandoc
  required_pandoc <- "1.12.3"
  if (!pandoc_available(required_pandoc)) {
    stop("pandoc version ", required_pandoc, " or higher ",
         "is required and was not found.", call. = FALSE)
  }

  # setup a cleanup function for intermediate files
  intermediates <- c()
  on.exit(lapply(intermediates,
                 function(f) {
                   if (clean && file.exists(f))
                     unlink(f, recursive = TRUE)
                 }),
          add = TRUE)

  # execute within the input file's directory
  oldwd <- setwd(dirname(tools::file_path_as_absolute(input)))
  on.exit(setwd(oldwd), add = TRUE)

  # reset the name of the input file to be relative
  input <- basename(input)

  # read the input file
  input_lines <- read_lines_utf8(input, encoding)

  # if we haven't been passed a fully formed output format then
  # resolve it by looking at the yaml
  if (!is_output_format(output.format))
    output.format <- output_format_from_yaml(output.format, input_lines)

  # automatically create an output file name if necessary
  if (is.null(output.file))
    output.file <- pandoc_output_file(input, output.format$pandoc$to)

  # use output filename based files dir
  files_dir <- knitr_files_dir(basename(output.file))

  # call any filter that's been specified
  if (!is.null(output.format$filter)) {
    output.format <- output.format$filter(output.format = output.format,
                                          files.dir = files_dir,
                                          input.lines = input_lines)
  }

  # knit if necessary
  if (tolower(tools::file_ext(input)) %in% c("rmd", "rmarkdown")) {

    # default rendering and chunk options
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)

    # use filename based figure and cache directories
    figures_dir <- paste(files_dir,
                         "/figure-", output.format$pandoc$to, "/",
                         sep = "")
    knitr::opts_chunk$set(fig.path=figures_dir)
    knitr::opts_chunk$set(cache.path=paste(knitr_cache_dir(input), "/", sep=""))

    # merge user options and hooks
    if (!is.null(output.format$knitr)) {
      knitr::opts_knit$set(as.list(output.format$knitr$opts_knit))
      knitr::opts_chunk$set(as.list(output.format$knitr$opts_chunk))
      knitr::knit_hooks$set(as.list(output.format$knitr$knit_hooks))
    }

    # calculate the output file name
    knit_output <- file_with_meta_ext(input, "knit", "md")

    # perform the knit
    input <- knitr::knit(input,
                         knit_output,
                         envir = envir,
                         quiet = quiet,
                         encoding = encoding)

    # add md file to intermediates
    intermediates <- c(intermediates, input)

    # clean the files_dir if we've either been asking to clean supporting
    # files or if we know the supporting files are going to get copied
    # to an output directory
    if (output.format$clean.supporting ||
        (dirname(input) != dirname(output.file))) {
       intermediates <- c(intermediates, files_dir)
    }
  }

  # if the encoding isn't UTF-8 then write a UTF-8 version
  if (!identical(encoding, "UTF-8")) {
    input_text <- read_lines_utf8(input, encoding)
    input <- file_with_meta_ext(input, "utf8")
    writeLines(input_text, input, useBytes = TRUE)

    # add utf8 version to intermediates
    intermediates <- c(intermediates, input)
  }

  # copy supporting files to the output directory if necessary
  if (!output.format$clean.supporting) {
    if (dirname(input) != dirname(output.file)) {
      file.copy(from = files_dir,
                to = dirname(output.file),
                recursive = TRUE)
    }
  }

  # run the conversion
  pandoc_convert(input,
                 output.format$pandoc$to,
                 output.format$pandoc$from,
                 output.file,
                 TRUE,
                 output.format$pandoc$args,
                 !quiet)

  # return the full path to the output file
  invisible(tools::file_path_as_absolute(output.file))
}


#' Render supporting files for an input document
#'
#' Render (copy) required supporting files for an input document to the _files
#' directory associated with the document.
#'
#' @param from Directory to copy from
#' @param to Directory to copy files into
#' @param rename.to Optional rename of source directory after it is copied
#'
#' @return The relative path to the supporting files. This path is suitable
#' for inclusion in HTML\code{href} and \code{src} attributes.
#'
#' @export
render_supporting_files <- function(from, to, rename.to = NULL) {

  # auto-create directory for supporting files
  if (!file.exists(to))
    dir.create(to)

  # target directory is based on the dirname of the path or the rename.to
  # value if it was provided
  target_stage_dir <- file.path(to, basename(from))
  target_dir <- file.path(to, ifelse(is.null(rename.to),
                                     basename(from),
                                     rename.to))

  # copy the directory if it hasn't already been copied
  if (!file.exists(target_dir) && !file.exists(target_stage_dir)) {
    file.copy(from = from,
              to = to,
              recursive = TRUE)
    if (!is.null(rename.to)) {
      file.rename(from = target_stage_dir,
                  to = target_dir)
    }
  }

  # return the target dir (used to form links in the HTML)
  target_dir
}



