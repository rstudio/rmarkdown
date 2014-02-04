
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
                   if (file.exists(f))
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
  if (is.null(output.file)) {

    # compute output file
    output.file <- pandoc_output_file(input, output.format$pandoc$to)

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
  if (!is.null(output.format$filter))
    output.format <- output.format$filter(output.format,
                                          output.file,
                                          input_lines)

  # knit if necessary
  if (tolower(tools::file_ext(input)) %in% c("rmd", "rmarkdown")) {

    # default rendering and chunk options
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)

    # use filename based files and cache directories
    files_dir <- knitr_files_dir(basename(input))
    figures_dir <- paste(files_dir,
                         "figure-", output.format$pandoc$to, "/",
                         sep = "")
    knitr::opts_chunk$set(fig.path=figures_dir)
    knitr::opts_chunk$set(cache.path=knitr_cache_dir(input))

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

    # always clean the md file
    intermediates <- c(intermediates, input)

    # clean the files dir if requested
    if (output.format$clean.supporting)
       intermediates <- c(intermediates, files_dir)
  }

  # if the encoding isn't UTF-8 then write a UTF-8 version
  if (!identical(encoding, "UTF-8")) {
    input_text <- read_lines_utf8(input, encoding)
    input <- file_with_meta_ext(input, "utf8")
    writeLines(input_text, input, useBytes = TRUE)

    # always cleanup the utf8 version
    intermediates <- c(intermediates, input)
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


#' Copy supporting files for an input document
#'
#' Copy required supporting files for an input document to the _files
#' directory associated with the document.
#'
#' @param input Input document to copy supporting files for
#' @param path Directory to copy files from
#' @param rename.to Optional rename of source directory after it is copied
#'
#' @return The relative path to the supporting files. This path is suitable
#' for inclusion in HTML\code{href} and \code{src} attributes.
#'
#' @export
copy_supporting_files <- function(input, path, rename.to = NULL) {

  # directory for supporting files
  supporting_files <- knitr_files_dir(input)
  if (!file.exists(supporting_files))
    dir.create(supporting_files)

  # target directory is based on the dirname of the path or the rename.to
  # value if it was provided
  target_stage_dir <- file.path(supporting_files, basename(path))
  target_dir <- file.path(supporting_files, ifelse(is.null(rename.to),
                                                   basename(path),
                                                   rename.to))

  # copy the directory if it hasn't already been copied
  if (!file.exists(target_dir) && !file.exists(target_stage_dir)) {
    file.copy(from = path,
              to = supporting_files,
              recursive = TRUE)
    if (!is.null(rename.to)) {
      file.rename(from = target_stage_dir,
                  to = target_dir)
    }
  }

  # return the target dir (used to form links in the HTML)
  target_dir
}


