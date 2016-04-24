#' The YAML metadata of the current R Markdown document
#'
#' The object \code{metadata} stores the YAML metadata of the current R Markdown
#' document as a list, which you may use in the R code chunks, e.g.
#' \code{rmarkdown::metadata$title} (the title of the document),
#' \code{rmarkdown::metadata$author}, and \code{rmarkdown::metadata$foo} (if you
#' have a YAML field named \code{foo}), etc.
#' @usage NULL
#' @examples rmarkdown::metadata
#' @export
metadata <- list()

#' @export
render <- function(input,
                   output_format = NULL,
                   output_file = NULL,
                   output_dir = NULL,
                   output_options = NULL,
                   intermediates_dir = NULL,
                   runtime = c("auto", "static", "shiny"),
                   clean = TRUE,
                   params = NULL,
                   knit_meta = NULL,
                   envir = parent.frame(),
                   run_pandoc = TRUE,
                   quiet = FALSE,
                   encoding = getOption("encoding")) {

  perf_timer_start("render")

  # check for "all" output formats
  if (identical(output_format, "all")) {
    output_format <- enumerate_output_formats(input, envir, encoding)
    if (is.null(output_format))
      output_format <- "html_document"
  }

  # check for a list of output formats -- if there is more than one
  # then recursively call this function with each format by name
  if (is.character(output_format) && length(output_format) > 1) {
    outputs <- character()
    for (format in output_format) {
      # the output_file argument is intentionally ignored (we can't give
      # the same name to each rendered output); copy the rest by name
      output <- render(input = input,
                       output_format = format,
                       output_file = NULL,
                       output_dir = output_dir,
                       output_options = output_options,
                       intermediates_dir = intermediates_dir,
                       runtime = runtime,
                       clean = clean,
                       params = params,
                       knit_meta = knit_meta,
                       envir = envir,
                       run_pandoc = run_pandoc,
                       quiet = quiet,
                       encoding = encoding)
      outputs <- c(outputs, output)
    }
    if (length(output_file) > 1) {
      file.rename(outputs, output_file)
      outputs <- output_file
    }
    return(invisible(outputs))
  }

  # check for required version of pandoc
  required_pandoc <- "1.12.3"
  if (!pandoc_available(required_pandoc)) {
    stop("pandoc version ", required_pandoc, " or higher ",
         "is required and was not found.", call. = FALSE)
  }

  # setup a cleanup function for intermediate files
  intermediates <- c()
  on.exit(if (clean) unlink(intermediates, recursive = TRUE), add = TRUE)

  # ensure we have a directory to store intermediates
  if (!is.null(intermediates_dir)) {
    if (!dir_exists(intermediates_dir))
      dir.create(intermediates_dir, recursive = TRUE)
    intermediates_dir <- normalize_path(intermediates_dir)
  }
  intermediates_loc <- function(file) {
    if (is.null(intermediates_dir))
      file
    else
      file.path(intermediates_dir, file)
  }

  # resolve output directory before we change the working directory in
  # preparation for rendering the document
  if (!is.null(output_dir)) {
    if (!dir_exists(output_dir))
      dir.create(output_dir, recursive = TRUE)
    output_dir <- normalize_path(output_dir)
  }

  # remember the name of the original input document (we overwrite 'input' once
  # we've knitted)
  original_input <- normalize_path(input)

  # if the input file has shell characters in its name then make a copy that
  # doesn't have shell characters
  if (grepl(.shell_chars_regex, basename(input))) {
    # form the name of the file w/o shell characters
    input_no_shell_chars <- intermediates_loc(
        file_name_without_shell_chars(basename(input)))

    if (file.exists(input_no_shell_chars)) {
      stop("The name of the input file cannot contain the special shell ",
           "characters: ", .shell_chars_regex, " (attempted to copy to a ",
           "version without those characters '", input_no_shell_chars, "' ",
           "however that file already exists)", call. = FALSE)
    }
    file.copy(input, input_no_shell_chars, overwrite = TRUE)
    intermediates <- c(intermediates, input_no_shell_chars)
    input <- input_no_shell_chars

    # if an intermediates directory wasn't explicit before, make it explicit now
    if (is.null(intermediates_dir)) {
      intermediates_dir <-
        dirname(normalize_path(input_no_shell_chars))
      # never use the original input directory as the intermediate directory,
      # otherwise external resources discovered will be deleted as intermediate
      # files later (because they are copied to the "intermediate" dir)
      if (same_path(intermediates_dir, dirname(original_input)))
        intermediates_dir <- NULL
    }
  }

  # execute within the input file's directory
  oldwd <- setwd(dirname(tools::file_path_as_absolute(input)))
  on.exit(setwd(oldwd), add = TRUE)

  # reset the name of the input file to be relative and calculate variations
  # on the filename for our various intermediate targets
  input <- basename(input)
  knit_input <- input
  knit_output <- intermediates_loc(file_with_meta_ext(input, "knit", "md"))

  intermediates <- c(intermediates, knit_output)
  utf8_input <- intermediates_loc(file_with_meta_ext(input, "utf8", "md"))
  intermediates <- c(intermediates, utf8_input)

  # track whether this was straight markdown input (to prevent keep_md later)
  md_input <- identical(tolower(tools::file_ext(input)), "md")

  # if this is an R script then spin it first
  if (identical(tolower(tools::file_ext(input)), "r")) {
    # make a copy of the file to spin
    spin_input <- intermediates_loc(file_with_meta_ext(input, "spin", "R"))
    file.copy(input, spin_input, overwrite = TRUE)
    intermediates <- c(intermediates, spin_input)
    # spin it
    spin_rmd <- knitr::spin(spin_input,
                            knit = FALSE,
                            envir = envir,
                            format = "Rmd")
    intermediates <- c(intermediates, spin_rmd)
    knit_input <- spin_rmd
    # append default metadata (this will be ignored if there is user
    # metadata elsewhere in the file)
    metadata <- paste('\n',
      '---\n',
      'title: "', input, '"\n',
      'author: "', Sys.info()[["user"]], '"\n',
      'date: "', date(), '"\n',
      '---\n'
    , sep = "")
    if (!identical(encoding, "native.enc"))
      metadata <- iconv(metadata, to = encoding)
    cat(metadata, file = knit_input, append = TRUE)
  }

  # read the input file
  input_lines <- read_lines_utf8(knit_input, encoding)

  # read the yaml front matter
  yaml_front_matter <- parse_yaml_front_matter(input_lines)

  # if we haven't been passed a fully formed output format then
  # resolve it by looking at the yaml
  if (!is_output_format(output_format)) {
    output_format <- output_format_from_yaml_front_matter(input_lines,
                                                          output_options,
                                                          output_format)
    output_format <- create_output_format(output_format$name,
                                          output_format$options)
  }
  pandoc_to <- output_format$pandoc$to

  # determine whether we need to run citeproc (based on whether we
  # have references in the input)
  run_citeproc <- citeproc_required(yaml_front_matter, input_lines)

  # generate outpout file based on input filename
  if (is.null(output_file))
    output_file <- pandoc_output_file(input, output_format$pandoc)

  # if an output_dir was specified then concatenate it with the output file
  if (!is.null(output_dir)) {
    output_file <- file.path(output_dir, basename(output_file))
  }
  output_dir <- dirname(output_file)

  # use output filename based files dir
  files_dir <-file.path(output_dir, knitr_files_dir(basename(output_file)))
  files_dir <- pandoc_path_arg(files_dir)

  # default to no cache_dir (may be generated by the knit)
  cache_dir <- NULL

  # call any intermediate files generator, if we have an intermediates directory
  # (do this before knitting in case the knit requires intermediates)
  if (!is.null(intermediates_dir) &&
      !is.null(output_format$intermediates_generator)) {
    intermediates <- c(intermediates,
                       output_format$intermediates_generator(original_input,
                                                             encoding,
                                                             intermediates_dir))
  }

  # reset knit_meta (and ensure it's always reset before exiting render)
  knit_meta_reset()
  on.exit(knit_meta_reset(), add = TRUE)

  # presume that we're rendering as a static document unless specified
  # otherwise in the parameters
  runtime <- match.arg(runtime)
  if (identical(runtime, "auto")) {
    if (!is.null(yaml_front_matter$runtime))
      runtime <- yaml_front_matter$runtime
    else
      runtime <- "static"
  }

  # call any pre_knit handler
  if (!is.null(output_format$pre_knit)) {
    output_format$pre_knit(input = original_input)
  }

  # knit if necessary
  if (tolower(tools::file_ext(input)) %in% c("r", "rmd", "rmarkdown")) {

    # restore options and hooks after knit
    optk <- knitr::opts_knit$get()
    on.exit(knitr::opts_knit$restore(optk), add = TRUE)
    optc <- knitr::opts_chunk$get()
    on.exit(knitr::opts_chunk$restore(optc), add = TRUE)
    hooks <- knitr::knit_hooks$get()
    on.exit(knitr::knit_hooks$restore(hooks), add = TRUE)
    ohooks <- knitr::opts_hooks$get()
    on.exit(knitr::opts_hooks$restore(ohooks), add = TRUE)
    templates <- knitr::opts_template$get()
    on.exit(knitr::opts_template$restore(templates), add = TRUE)

    # default rendering and chunk options
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)

    # store info about the final output format in opts_knit
    knitr::opts_knit$set(
      rmarkdown.pandoc.from = output_format$pandoc$from,
      rmarkdown.pandoc.to = pandoc_to,
      rmarkdown.keep_md = output_format$keep_md,
      rmarkdown.version = 2
    )

    # trim whitespace from around source code
    if (utils::packageVersion("knitr") < "1.5.23") {
      local({
        hook_source = knitr::knit_hooks$get('source')
        knitr::knit_hooks$set(source = function(x, options) {
          hook_source(strip_white(x), options)
        })
      })
    }

    # use filename based figure and cache directories
    figures_dir <- paste(files_dir, "/figure-", pandoc_to, "/", sep = "")
    knitr::opts_chunk$set(fig.path=figures_dir)
    cache_dir <-knitr_cache_dir(input, pandoc_to)
    knitr::opts_chunk$set(cache.path=cache_dir)

    # strip the trailing slash from cache_dir so that file.exists() and unlink()
    # check on it later works on windows
    cache_dir <- gsub("/$", "", cache_dir)

    # merge user options and hooks
    if (!is.null(output_format$knitr)) {
      knitr::opts_knit$set(as.list(output_format$knitr$opts_knit))
      knitr::opts_chunk$set(as.list(output_format$knitr$opts_chunk))
      knitr::opts_template$set(as.list(output_format$knitr$opts_template))
      knitr::knit_hooks$set(as.list(output_format$knitr$knit_hooks))
      knitr::opts_hooks$set(as.list(output_format$knitr$opts_hooks))
    }

    # setting the runtime (static/shiny) type
    knitr::opts_knit$set(rmarkdown.runtime = runtime)

    # make the params available within the knit environment
    # (only do this if there are parameters in the front matter
    # so we don't require recent knitr for all users)
    if (!is.null(yaml_front_matter$params)) {

      params <- knit_params_get(input_lines, params)

      # make the params available in the knit environment
      if (!exists("params", envir = envir, inherits = FALSE)) {
        assign("params", params, envir = envir)
        lockBinding("params", envir)
        on.exit({
          do.call("unlockBinding", list("params", envir))
          remove("params", envir = envir)
        }, add = TRUE)
      } else {
        stop("params object already exists in knit environment ",
             "so can't be overwritten by render params", call. = FALSE)
      }
    }

    # make the yaml_front_matter available as 'metadata' within the
    # knit environment (unless it is already defined there in which case
    # we emit a warning)
    env <- environment(render)
    metadata_this <- env$metadata
    do.call("unlockBinding", list("metadata", env))
    on.exit({
      if (bindingIsLocked("metadata", env)) {
        do.call("unlockBinding", list("metadata", env))
      }
      env$metadata <- metadata_this
      lockBinding("metadata", env)
    }, add = TRUE)
    env$metadata <- yaml_front_matter

    perf_timer_start("knitr")

    # perform the knit
    input <- knitr::knit(knit_input,
                         knit_output,
                         envir = envir,
                         quiet = quiet,
                         encoding = encoding)

    perf_timer_stop("knitr")
  }

  # call any post_knit handler
  if (!is.null(output_format$post_knit)) {
    post_knit_extra_args <- output_format$post_knit(yaml_front_matter,
                                                    knit_input,
                                                    runtime)
    output_format$pandoc$args <- c(output_format$pandoc$args, post_knit_extra_args)
  }

  # pull any R Markdown warnings from knit_meta and emit
  rmd_warnings <- knit_meta_reset(class = "rmd_warning")
  for (rmd_warning in rmd_warnings) {
    message("Warning: ", rmd_warning)
  }

  # collect remaining knit_meta
  knit_meta <- knit_meta_reset()

  # if this isn't html and there are html dependencies then flag an error
  if (!(is_pandoc_to_html(output_format$pandoc) ||
        identical(tolower(tools::file_ext(output_file)), "html")))  {
    if (has_html_dependencies(knit_meta)) {
      if (!isTRUE(yaml_front_matter$always_allow_html)) {
        stop("Functions that produce HTML output found in document targeting ",
             pandoc_to, " output.\nPlease change the output type ",
             "of this document to HTML. Alternatively, you can allow\n",
             "HTML output in non-HTML formats by adding this option to the YAML front",
             "-matter of\nyour rmarkdown file:\n\n",
             "  always_allow_html: yes\n\n",
             "Note however that the HTML output will not be visible in non-HTML formats.\n\n",
             call. = FALSE)
      }
    }
    if (!identical(runtime, "static")) {
      stop("Runtime '", runtime, "' is not supported for ",
           pandoc_to, " output.\nPlease change the output type ",
           "of this document to HTML.", call. = FALSE)
    }
  }

  # clean the files_dir if we've either been asking to clean supporting files or
  # the knitr cache is active
  if (output_format$clean_supporting && (is.null(cache_dir) || !dir_exists(cache_dir)))
      intermediates <- c(intermediates, files_dir)

  # read the input text as UTF-8 then write it back out
  input_text <- read_lines_utf8(input, encoding)
  writeLines(input_text, utf8_input, useBytes = TRUE)

  if (run_pandoc) {

    perf_timer_start("pre-processor")

    # call any pre_processor
    if (!is.null(output_format$pre_processor)) {
      extra_args <- output_format$pre_processor(yaml_front_matter,
                                                utf8_input,
                                                runtime,
                                                knit_meta,
                                                files_dir,
                                                output_dir)
      output_format$pandoc$args <- c(output_format$pandoc$args, extra_args)
    }

    perf_timer_stop("pre-processor")

    need_bibtex <- grepl('[.](pdf|tex)$', output_file) &&
      any(c('--natbib', '--biblatex') %in% output_format$pandoc$args)
    # if we are running citeproc then explicitly forward the bibliography
    # on the command line (works around pandoc-citeproc issue whereby yaml
    # strings that begin with numbers are interpreted as numbers)
    if (!is.null(bibliography <- yaml_front_matter$bibliography)) {
      # remove the .bib extension since it does not work with MikTeX's BibTeX
      if (need_bibtex && is_windows()) bibliography <- sub('[.]bib$', '', bibliography)
      output_format$pandoc$args <- c(output_format$pandoc$args,
                                     rbind("--bibliography", pandoc_path_arg(bibliography)))
    }

    perf_timer_start("pandoc")

    convert <- function(output, citeproc = FALSE) {
      pandoc_convert(
        utf8_input, pandoc_to, output_format$pandoc$from, output, citeproc,
        output_format$pandoc$args, !quiet
      )
    }
    texfile <- file_with_ext(output_file, "tex")
    # compile Rmd to tex when we need to generate bibliography with natbib/biblatex
    if (need_bibtex) {
      convert(texfile)
      # manually compile tex if PDF output is expected
      if (grepl('[.]pdf$', output_file)) {
        latexmk(texfile, output_format$pandoc$latex_engine)
        file.rename(file_with_ext(texfile, "pdf"), output_file)
      }
      # clean up the tex file if necessary
      if ((texfile != output_file) && !output_format$pandoc$keep_tex)
        on.exit(unlink(texfile), add = TRUE)
    } else {
      # run the main conversion if the output file is not .tex
      convert(output_file, run_citeproc)
      # run conversion again to .tex if we want to keep the tex source
      if (output_format$pandoc$keep_tex) convert(texfile, run_citeproc)
    }

    # pandoc writes the output alongside the input, so if we rendered from an
    # intermediate directory, move the output file
    if (!is.null(intermediates_dir)) {
      intermediate_output <- file.path(intermediates_dir, basename(output_file))
      if (file.exists(intermediate_output)) {
        file.rename(intermediate_output, output_file)
      }
    }

    perf_timer_stop("pandoc")

    perf_timer_start("post-processor")

    # if there is a post-processor then call it
    if (!is.null(output_format$post_processor))
      output_file <- output_format$post_processor(yaml_front_matter,
                                                  utf8_input,
                                                  output_file,
                                                  clean,
                                                  !quiet)

    if (!quiet) {
      message("\nOutput created: ", relative_to(oldwd, output_file))
    }

    perf_timer_stop("post-processor")

  }

  perf_timer_stop("render")

  # write markdown output if requested
  if (output_format$keep_md && !md_input) {

    md <- c(md_header_from_front_matter(yaml_front_matter),
            partition_yaml_front_matter(input_text)$body)

    writeLines(md, file_with_ext(output_file, "md"), useBytes = TRUE)
  }

  if (run_pandoc) {
    # return the full path to the output file
    invisible(tools::file_path_as_absolute(output_file))
  } else {
    # did not run pandoc; returns the markdown output with attributes of the
    # knitr meta data and intermediate files
    structure(input, knit_meta = knit_meta, intermediates = intermediates)
  }
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
  if (!dir_exists(files_dir))
    dir.create(files_dir)

  # target directory is based on the dirname of the path or the rename_to
  # value if it was provided
  target_stage_dir <- file.path(files_dir, basename(from))
  target_dir <- file.path(files_dir, ifelse(is.null(rename_to),
                                     basename(from),
                                     rename_to))

  # copy the directory if it hasn't already been copied
  if (!dir_exists(target_dir) && !dir_exists(target_stage_dir)) {
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

# reset knitr meta output (returns any meta output generated since the last
# call to knit_meta_reset), optionally scoped to a specific output class
knit_meta_reset <- function(class = NULL) {
  if (utils::packageVersion("knitr") >= "1.5.26")
    knitr::knit_meta(class, clean = TRUE)
  else
    NULL
}

md_header_from_front_matter <- function(front_matter) {

  md <- c()

  if (!is.null(front_matter$title))
    md <- c(md, paste("# ", front_matter$title, sep = ""))

  if (is.character(front_matter$author)) {
    authors <- paste(front_matter$author, "  ", sep = "")
    md <- c(md, authors)
  }

  if (!is.null(front_matter$date))
    md <- c(md, paste(front_matter$date, "  ", sep = ""))

  md
}




