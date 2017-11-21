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
                   knit_root_dir = NULL,
                   runtime =  c("auto", "static", "shiny", "shiny_prerendered"),
                   clean = TRUE,
                   params = NULL,
                   knit_meta = NULL,
                   envir = parent.frame(),
                   run_pandoc = TRUE,
                   quiet = FALSE,
                   encoding = getOption("encoding")) {

  perf_timer_start("render")

  init_render_context()
  on.exit(clear_render_context(), add = TRUE)

  # render() may call itself, e.g., in discover_rmd_resources(); in this case,
  # we should not clean up temp files in the nested render() call, but wait
  # until the top-level render() exits to clean up temp files
  .globals$level <- .globals$level + 1L  # increment level in a nested render()
  on.exit({
    .globals$level <- .globals$level - 1L
    if (.globals$level == 0) clean_tmpfiles()
  }, add = TRUE)

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
                       knit_root_dir = knit_root_dir,
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

  # check for required version of pandoc if we are running pandoc
  if (run_pandoc) {
    required_pandoc <- "1.12.3"
    pandoc_available(required_pandoc, error = TRUE)
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

  # check whether this document requires a knit
  requires_knit <- tolower(tools::file_ext(input)) %in% c("r", "rmd", "rmarkdown")

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

  # force evaluation of knitr root dir before we change directory context
  force(knit_root_dir)

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

  # if this is shiny_prerendered then modify the output format to
  # be single-page and to output dependencies to the shiny.dep file
  shiny_prerendered_dependencies <- list()
  if (requires_knit && is_shiny_prerendered(yaml_front_matter$runtime)) {

    # first validate that the user hasn't passed an already created output_format
    if (is_output_format(output_format)) {
      stop("You cannot pass a fully constructed output_format to render when ",
           "using runtime: shiny_prerendered")
    }

    # require shiny for the knit
    if (requireNamespace("shiny")) {
      if (!"package:shiny" %in% search())
        attachNamespace("shiny")
    }
    else
      stop("The shiny package is required for 'shiny_prerendered' documents")

    # force various output options
    output_options$self_contained <- FALSE
    output_options$dependency_resolver <- function(deps) {
      shiny_prerendered_dependencies <<- deps
      list()
    }
  }

  # if we haven't been passed a fully formed output format then
  # resolve it by looking at the yaml
  if (!is_output_format(output_format)) {
    output_format <- output_format_from_yaml_front_matter(input_lines,
                                                          output_options,
                                                          output_format,
                                                          encoding = encoding)
    output_format <- create_output_format(output_format$name,
                                          output_format$options)
  }
  pandoc_to <- output_format$pandoc$to

  # generate outpout file based on input filename
  if (is.null(output_file))
    output_file <- pandoc_output_file(input, output_format$pandoc)

  # if an output_dir was specified then concatenate it with the output file
  if (!is.null(output_dir)) {
    output_file <- file.path(output_dir, basename(output_file))
  }
  output_dir <- dirname(output_file)

  # use output filename based files dir
  files_dir <- file.path(output_dir, knitr_files_dir(basename(output_file)))
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
  old_knit_meta <- knit_meta_reset()
  on.exit({
    knit_meta_reset()
    if (length(old_knit_meta)) {
      knitr::knit_meta_add(old_knit_meta, attr(old_knit_meta, 'knit_meta_id'))
    }
  }, add = TRUE)

  # presume that we're rendering as a static document unless specified
  # otherwise in the parameters
  runtime <- match.arg(runtime)
  if (identical(runtime, "auto")) {
    if (!is.null(yaml_front_matter$runtime))
      runtime <- yaml_front_matter$runtime
    else
      runtime <- "static"
  }

  # set df_print
  context <- render_context()
  context$df_print <- resolve_df_print(output_format$df_print)

  # call any pre_knit handler
  if (!is.null(output_format$pre_knit)) {
    output_format$pre_knit(input = original_input)
  }

  # function used to call post_knit handler
  call_post_knit_handler <- function() {
    if (!is.null(output_format$post_knit)) {
      post_knit_extra_args <- output_format$post_knit(yaml_front_matter,
                                                      knit_input,
                                                      runtime,
                                                      encoding = encoding)
    } else {
      post_knit_extra_args <- NULL
    }
    c(output_format$pandoc$args, post_knit_extra_args)
  }

  # determine our id-prefix (add one if necessary for runtime: shiny)
  id_prefix <- id_prefix_from_args(output_format$pandoc$args)
  if (!nzchar(id_prefix) && is_shiny(runtime)) {
    id_prefix <- "section-"
    output_format$pandoc$args <- c(output_format$pandoc$args, rbind("--id-prefix", id_prefix))
  }

  # knit if necessary
  if (requires_knit) {

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

    # run render on_exit (run after the knit hooks are saved so that
    # any hook restoration can take precedence)
    if (is.function(output_format$on_exit))
      on.exit(output_format$on_exit(), add = TRUE)

    # default rendering and chunk options
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)

    # store info about the final output format in opts_knit
    knitr::opts_knit$set(
      rmarkdown.pandoc.from = output_format$pandoc$from,
      rmarkdown.pandoc.to = pandoc_to,
      rmarkdown.pandoc.id_prefix = id_prefix,
      rmarkdown.keep_md = output_format$keep_md,
      rmarkdown.df_print = output_format$df_print,
      rmarkdown.version = 2,
      rmarkdown.runtime = runtime
    )

    # read root directory from argument (has precedence) or front matter
    root_dir <- knit_root_dir
    if (is.null(root_dir))
      root_dir <- yaml_front_matter$knit_root_dir
    if (!is.null(root_dir))
      knitr::opts_knit$set(root.dir = root_dir)

    # use filename based figure and cache directories
    base_pandoc_to <- gsub('[-+].*', '', pandoc_to)
    if (base_pandoc_to == 'html4') base_pandoc_to <- 'html'
    figures_dir <- paste(files_dir, "/figure-", base_pandoc_to, "/", sep = "")
    knitr::opts_chunk$set(fig.path = figures_dir)
    cache_dir <- knitr_cache_dir(input, base_pandoc_to)
    knitr::opts_chunk$set(cache.path = cache_dir)

    # strip the trailing slash from cache_dir so that file.exists() and unlink()
    # check on it later works on windows
    cache_dir <- gsub("/$", "", cache_dir)

    # merge user options and hooks
    if (!is.null(output_format$knitr)) {
      knitr::opts_knit$set(as.list(output_format$knitr$opts_knit))
      knitr::opts_chunk$set(adjust_dev(as.list(output_format$knitr$opts_chunk)))
      knitr::opts_template$set(as.list(output_format$knitr$opts_template))
      knitr::knit_hooks$set(as.list(output_format$knitr$knit_hooks))
      knitr::opts_hooks$set(as.list(output_format$knitr$opts_hooks))
    }

    # setting the runtime (static/shiny) type
    knitr::opts_knit$set(rmarkdown.runtime = runtime)

    # install evaluate hook for shiny_prerendred
    if (is_shiny_prerendered(runtime)) {

      # remove uncached .RData (will be recreated from context="data" chunks)
      shiny_prerendered_remove_uncached_data(original_input)

      # set the cache option hook and evaluate hook
      knitr::opts_hooks$set(label = shiny_prerendered_option_hook(original_input,encoding))
      knitr::knit_hooks$set(evaluate = shiny_prerendered_evaluate_hook(original_input))
    }

    # install global chunk handling for runtime: shiny (evaluate the 'global'
    # chunk only once, and in the global environment)
    if (is_shiny_classic(runtime) && !is.null(shiny::getDefaultReactiveDomain())) {

      # install evaluate hook to ensure that the 'global' chunk for this source
      # file is evaluated only once and is run outside of a user reactive domain
      knitr::knit_hooks$set(evaluate = function(code, envir, ...) {

        # check for 'global' chunk label
        if (identical(knitr::opts_current$get("label"), "global")) {

          # check list of previously evaludated global chunks
          code_string <- paste(code, collapse = '\n')
          if (!code_string %in% .globals$evaluated_global_chunks) {

            # save it in our list of evaluated global chunks
            .globals$evaluated_global_chunks <-
              c(.globals$evaluated_global_chunks, code_string)

            # evaluate with no reactive domain to prevent any shiny code (e.g.
            # a reactive timer) from attaching to the current user session
            # (resulting in it's destruction when that session ends)
            shiny::withReactiveDomain(NULL, {
              evaluate::evaluate(code, envir = globalenv(), ...)
            })

          } else {
            list()
          }
        # delegate to standard evaluate for everything else
        } else {
          evaluate::evaluate(code, envir, ...)
        }
      })
    }

    # make the params available within the knit environment
    # (only do this if there are parameters in the front matter
    # so we don't require recent knitr for all users)
    if (!is.null(yaml_front_matter$params)) {

      params <- knit_params_get(input_lines, params)

      # bail if an object called 'params' exists in this environment,
      # and it seems to be an unrelated user-created object. store
      # references so we can restore them post-render
      hasParams <- exists("params", envir = envir, inherits = FALSE)
      envirParams <- NULL

      if (hasParams) {
        envirParams <- get("params", envir = envir, inherits = FALSE)
        isKnownParamsObject <-
          inherits(envirParams, "knit_param_list") ||
          inherits(envirParams, "knit_param")

        if (!isKnownParamsObject) {
          stop("params object already exists in knit environment ",
               "so can't be overwritten by render params", call. = FALSE)
        }
      }

      # make the params available in the knit environment
      assign("params", params, envir = envir)
      lockBinding("params", envir)
      on.exit({
        do.call("unlockBinding", list("params", envir))
        if (hasParams)
          assign("params", envirParams, envir = envir)
        else
          remove("params", envir = envir)
      }, add = TRUE)
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

    # call onKnit hooks (normalize to list)
    sapply(as.list(getHook("rmarkdown.onKnit")), function(hook) {
      tryCatch(hook(input = original_input), error = function(e) NULL)
    })
    on.exit({
      sapply(as.list(getHook("rmarkdown.onKnitCompleted")), function(hook) {
        tryCatch(hook(input = original_input), error = function(e) NULL)
      })
    }, add = TRUE)

    perf_timer_start("knitr")

    # perform the knit
    input <- knitr::knit(knit_input,
                         knit_output,
                         envir = envir,
                         quiet = quiet,
                         encoding = encoding)

    perf_timer_stop("knitr")

    # call post_knit handler
    output_format$pandoc$args <- call_post_knit_handler()

    # pull any R Markdown warnings from knit_meta and emit
    rmd_warnings <- knit_meta_reset(class = "rmd_warning")
    for (rmd_warning in rmd_warnings) {
      message("Warning: ", rmd_warning)
    }

    # pull out shiny_prerendered_contexts and append them as script tags
    shiny_prerendered_append_contexts(runtime, input, encoding)

    # collect remaining knit_meta
    knit_meta <- knit_meta_reset()

  } else {
    output_format$pandoc$args <- call_post_knit_handler()
  }

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

    # write shiny_prerendered_dependencies if we have them
    if (is_shiny_prerendered(runtime)) {
      shiny_prerendered_append_dependencies(utf8_input,
                                            shiny_prerendered_dependencies,
                                            files_dir,
                                            output_dir)
    }

    perf_timer_stop("pre-processor")

    need_bibtex <- grepl('[.](pdf|tex)$', output_file) &&
      any(c('--natbib', '--biblatex') %in% output_format$pandoc$args)

    perf_timer_start("pandoc")

    convert <- function(output, citeproc = FALSE) {

      # temporarily move figures to the intermediate dir if specified:
      # https://github.com/rstudio/rmarkdown/issues/500
      figures_dir <- gsub('/$', '', knitr::opts_chunk$get("fig.path"))
      if (!is.null(intermediates_dir) && dir_exists(figures_dir)) {
        figures_dir_tmp <- intermediates_loc(figures_dir)
        move_dir(figures_dir, figures_dir_tmp)
        on.exit(move_dir(figures_dir_tmp, figures_dir), add = TRUE)
      }

      # ensure we expand paths (for Windows where leading `~/` does
      # not get expanded by pandoc)
      utf8_input <- path.expand(utf8_input)
      output     <- path.expand(output)

      # if we don't detect any invalid shell characters in the
      # target path, then just call pandoc directly
      if (!grepl(.shell_chars_regex, output) && !grepl(.shell_chars_regex, utf8_input)) {
        return(pandoc_convert(
          utf8_input, pandoc_to, output_format$pandoc$from, output,
          citeproc, output_format$pandoc$args, !quiet
        ))
      }

      # render to temporary file (preserve extension)
      # this also ensures we don't pass a file path with invalid
      # characters to our pandoc invocation
      file_ext <- tools::file_ext(output)
      ext <- if (nzchar(file_ext))
        paste(".", file_ext, sep = "")
      else
        ""

      # render to a path in the current working directory
      # (avoid passing invalid characters to shell)
      pandoc_output_tmp <- basename(tempfile("pandoc", tmpdir = getwd(), fileext = ext))

      # clean up temporary file on exit
      on.exit(unlink(pandoc_output_tmp), add = TRUE)

      # call pandoc to render file
      status <- pandoc_convert(
        utf8_input, pandoc_to, output_format$pandoc$from, pandoc_output_tmp,
        citeproc, output_format$pandoc$args, !quiet
      )

      # construct output path (when passed only a file name to '--output',
      # pandoc seems to render in the same directory as the input file)
      pandoc_output_tmp_path <- file.path(dirname(utf8_input), pandoc_output_tmp)

      # rename output file to desired location
      renamed <- suppressWarnings(file.rename(pandoc_output_tmp_path, output))

      # rename can fail if the temporary directory and output path
      # lie on different volumes; in such a case attempt a file copy
      # see: https://github.com/rstudio/rmarkdown/issues/705
      if (!renamed) {
        copied <- file.copy(pandoc_output_tmp_path, output, overwrite = TRUE)
        if (!copied) {
          stop("failed to copy rendered pandoc artefact to '", output, "'")
        }
      }

      # return status
      status
    }
    texfile <- file_with_ext(output_file, "tex")
    # compile Rmd to tex when we need to generate bibliography with natbib/biblatex
    if (need_bibtex) {
      convert(texfile)
      # manually compile tex if PDF output is expected
      if (grepl('[.]pdf$', output_file)) {
        latexmk(texfile, output_format$pandoc$latex_engine, '--biblatex' %in% output_format$pandoc$args)
        file.rename(file_with_ext(texfile, "pdf"), output_file)
      }
      # clean up the tex file if necessary
      if ((texfile != output_file) && !output_format$pandoc$keep_tex)
        on.exit(unlink(texfile), add = TRUE)
    } else {
      # determine whether we need to run citeproc (based on whether we
      # have references in the input)
      run_citeproc <- citeproc_required(yaml_front_matter, input_lines)
      # generate .tex if we want to keep the tex source
      if (texfile != output_file && output_format$pandoc$keep_tex)
        convert(texfile, run_citeproc)
      # run the main conversion if the output file is not .tex
      convert(output_file, run_citeproc)
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
    file.copy(input, file_with_ext(output_file, "md"), overwrite = TRUE)
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
              recursive = TRUE,
              copy.mode = FALSE)
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
  knitr::knit_meta(class, clean = TRUE)
}

# render context (render-related state can be stuffed here)
.render_context <- NULL # initialized in .onLoad
render_context <- function() {
  .render_context$peek()
}

init_render_context <- function() {
  .render_context$push(new_render_context())
}

clear_render_context <- function() {
  .render_context$pop()
}

new_render_context <- function() {
  env <- new.env(parent = emptyenv())
  env$chunk.index <- 1
  env
}

merge_render_context <- function(context) {
  elements <- ls(envir = render_context(), all.names = TRUE)
  for (el in elements)
    context[[el]] <- get(el, envir = render_context())
  context
}


id_prefix_from_args <- function(args) {

  # scan for id-prefix argument
  for (i in 1:length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--id-prefix") && (i < length(args)))
      return(args[[i + 1]])
  }

  # default to empty string
  ""
}


resolve_df_print <- function(df_print) {

  # available methods
  valid_methods <- c("default", "kable", "tibble", "paged")

  # if we are passed NULL then select the first method
  if (is.null(df_print))
    df_print <- valid_methods[[1]]

  # if we are passed all of valid_methods then select the first one
  if (identical(valid_methods, df_print))
    df_print <- valid_methods[[1]]

  if (!is.function(df_print)) {
    if (df_print == "kable")
      df_print <- knitr::kable
    else if (df_print == "tibble") {
      if (!requireNamespace("tibble", quietly = TRUE))
        stop("Printing 'tibble' without 'tibble' package available")

      df_print <- function(x) print(tibble::as_tibble(x))
    }
    else if (df_print == "paged")
      df_print <- function(x) {
        if (!identical(knitr::opts_current$get("paged.print"), FALSE)) {
          knitr::asis_output(paged_table_html(x))
        }
        else {
          print(x)
        }
      }
    else if (df_print == "default")
      df_print <- print
    else
      stop('Invalid value for df_print (valid values are ',
           paste(valid_methods, collapse = ", "), call. = FALSE)
  }

  df_print
}


# package level globals
.globals <- new.env(parent = emptyenv())
.globals$evaluated_global_chunks <- character()
.globals$level <- 0L

