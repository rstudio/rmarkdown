
#' @export
render <- function(input,
                   output_format = NULL,
                   output_file = NULL,
                   output_options = NULL,
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

  # verify the input file doesn't have spaces in it's name
  if (grepl(' ', basename(input), fixed=TRUE))
    stop("The name of the input file must not contain spaces.")

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

  # reset the name of the input file to be relative and calculate variations
  # on the filename for our various intermediate targets
  input <- basename(input)
  knit_output <- file_with_meta_ext(input, "knit", "md")
  intermediates <- c(intermediates, knit_output)
  utf8_input <- file_with_meta_ext(input, "utf8", "md")
  intermediates <- c(intermediates, utf8_input)

  # read the input file
  input_lines <- read_lines_utf8(input, encoding)

  # if we haven't been passed a fully formed output format then
  # resolve it by looking at the yaml
  if (!is_output_format(output_format)) {
    output_format <- output_format_from_yaml_front_matter(input_lines,
                                                          output_options,
                                                          output_format)
    output_format <- create_output_format(output_format$name,
                                          output_format$options)
  }

  # automatically create an output file name if necessary
  if (is.null(output_file))
    output_file <- pandoc_output_file(input, output_format$pandoc$to)

  # use output filename based files dir
  files_dir <- knitr_files_dir(basename(output_file))

  # call any filter that's been specified
  if (!is.null(output_format$format_filter)) {
    output_format <- output_format$format_filter(output_format = output_format,
                                                 files_dir = files_dir,
                                                 input_lines = input_lines)
  }

  # knit if necessary
  if (tolower(tools::file_ext(input)) %in% c("rmd", "rmarkdown")) {

    # default rendering and chunk options
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)

    # use filename based figure and cache directories
    figures_dir <- paste(files_dir,
                         "/figure-", output_format$pandoc$to, "/",
                         sep = "")
    knitr::opts_chunk$set(fig.path=figures_dir)
    knitr::opts_chunk$set(cache.path=paste(knitr_cache_dir(input), "/", sep=""))

    # merge user options and hooks
    if (!is.null(output_format$knitr)) {
      knitr::opts_knit$set(as.list(output_format$knitr$opts_knit))
      knitr::opts_chunk$set(as.list(output_format$knitr$opts_chunk))
      knitr::knit_hooks$set(as.list(output_format$knitr$knit_hooks))
    }

    # perform the knit
    input <- knitr::knit(input,
                         knit_output,
                         envir = envir,
                         quiet = quiet,
                         encoding = encoding)

    # clean the files_dir if we've either been asking to clean supporting
    # files or if we know the supporting files are going to get copied
    # to an output directory
    if (output_format$clean_supporting ||
        (dirname(input) != dirname(output_file))) {
       intermediates <- c(intermediates, files_dir)
    }
  }

  # read the input text as UTF-8 then write it back out
  input_text <- read_lines_utf8(input, encoding)
  writeLines(input_text, utf8_input, useBytes = TRUE)

  # copy supporting files to the output directory if necessary
  if (!output_format$clean_supporting) {
    if (dirname(input) != dirname(output_file)) {
      file.copy(from = files_dir,
                to = dirname(output_file),
                recursive = TRUE)
    }
  }

  # run the conversion
  pandoc_convert(utf8_input,
                 output_format$pandoc$to,
                 output_format$pandoc$from,
                 output_file,
                 TRUE,
                 output_format$pandoc$args,
                 !quiet)

  # if there is a post-processor then call it
  if (!is.null(output_format$post_processor))
    output_format$post_processor(utf8_input, output_file, !quiet)

  if (!quiet)
    message("\nOutput created: ", output_file)

  # return the full path to the output file
  invisible(tools::file_path_as_absolute(output_file))
}


#' Render supporting files for an input document
#'
#' Render (copy) required supporting files for an input document to the _files
#' directory associated with the document.
#'
#' @param from Directory to copy from
#' @param files_dir Directory to copy files into
#' @param rename_to Optional rename of source directory after it is copied
#'
#' @return The relative path to the supporting files. This path is suitable
#' for inclusion in HTML\code{href} and \code{src} attributes.
#'
#' @export
render_supporting_files <- function(from, files_dir, rename_to = NULL) {

  # auto-create directory for supporting files
  if (!file.exists(files_dir))
    dir.create(files_dir)

  # target directory is based on the dirname of the path or the rename_to
  # value if it was provided
  target_stage_dir <- file.path(files_dir, basename(from))
  target_dir <- file.path(files_dir, ifelse(is.null(rename_to),
                                     basename(from),
                                     rename_to))

  # copy the directory if it hasn't already been copied
  if (!file.exists(target_dir) && !file.exists(target_stage_dir)) {
    file.copy(from = from,
              to = files_dir,
              recursive = TRUE)
    if (!is.null(rename_to)) {
      file.rename(from = target_stage_dir,
                  to = target_dir)
    }
  }

  # return the target dir (used to form links in the HTML)
  target_dir
}




