#' R Markdown Metadata
#'
#' Rmd files include a metadata section (typically located at the top of the
#' file) that can specify (among other things) the title, author, and date of
#' the document. Metadata adheres to the \href{https://yaml.org}{YAML} format
#' and is delimited by lines containing three dashes (\code{---}). Here is an
#' example metadata section:
#' \preformatted{---
#' title: "Crop Analysis Q3 2013"
#' author: Martha Smith
#' date: October 23rd, 2013
#' ---
#' }
#' Note that the \code{title} field is quoted. This is because titles often
#' contained embedded colons (\code{:}) and colons followed by a space need to
#' be quoted in YAML.
#' @details When title, author, and date metadata is provided it's used to
#'   automatically create a title section within output documents. If you don't
#'   want this section included in your document then you should remove the
#'   corresponding metadata fields.
#'
#'   When generating PDF and Beamer output there are also a number of other
#'   metadata fields that can be included to customize the appearance and theme
#'   of PDF output. For more details see the documentation for
#'   \code{\link{pdf_document}} and \code{\link{beamer_presentation}}.
#' @name rmd_metadata
NULL


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


#' Compiling R scripts to a notebook
#'
#' R Markdown can also compile R scripts to a notebook which includes
#' commentary, source code, and script output. Notebooks can be compiled to any
#' output format including HTML, PDF, and MS Word.
#'
#' @section Overview:
#'   To compile a notebook from an R script you simply pass the script to
#'   \code{\link{render}}. For example:
#'   \preformatted{
#' rmarkdown::render("analysis.R")
#' rmarkdown::render("analysis.R", "pdf_document")
#' }
#'   The first call to \code{\link{render}} creates an HTML document, whereas
#'   the second creates a PDF document.
#'
#'   By default the name of the script, username, and current date and time are
#'   included in the header of the generated notebook. You can override this
#'   default behavior by including explicit metadata in a specially formatted R
#'   comment:
#'   \preformatted{
#' #' ---
#' #' title: "Crop Analysis Q3 2013"
#' #' author: "John Smith"
#' #' date: "May 3rd, 2014"
#' #' ---
#' }
#' @section Including Markdown:
#'   Note that the R comment used above to add a title, author, and date
#'   includes a single-quote as a special prefix character. This is a
#'   \pkg{roxygen2} style comment, and it's actually possible to include many
#'   such comments in an R script, all of which will be converted to markdown
#'   content within the generated notebook. For example:
#'   \preformatted{#' A script comment that includes **markdown** formatting.}
#'   Rather than displaying as an R comment in the compiled notebook any
#'   \pkg{roxygen2} style comment will be treated as markdown and rendered
#'   accordingly.
#' @section knitr Spin:
#'   Including markdown within R comments is possible because \code{\link{render}}
#'   calls the \code{\link[knitr:spin]{knitr spin}} function to convert the R
#'   script to an Rmd file. The \code{spin} function also enables you to add
#'    knitr
#'   chunk options with another special comment prefix (\code{#+}).
#'
#'   Here's an example of a script that uses the various features of \code{spin}:
#'
#'   \url{https://github.com/yihui/knitr/blob/master/inst/examples/knitr-spin.R}
#'
#'   For more details on \code{knitr::spin} see the following documentation:
#'
#'   \url{https://yihui.org/knitr/demo/stitch/}
#' @name compile_notebook
NULL


#' Render R Markdown
#'
#' Render the input file to the specified output format using pandoc. If the
#' input requires knitting then \code{\link[knitr:knit]{knit}} is called prior
#' to pandoc.
#'
#' Note that the \pkg{knitr} \code{error} option is set to \code{FALSE} during
#' rendering (which is different from the \pkg{knitr} default value of
#' \code{TRUE}).
#'
#' For additional details on rendering R scripts see
#' \link[=compile_notebook]{Compiling R scripts to a notebook}.
#'
#' If no \code{output_format} parameter is specified then the output format is
#' read from the YAML front-matter of the input file. For example, the
#' following YAML would yield a PDF document:
#'
#' \preformatted{
#' output: pdf_document
#' }
#'
#' Additional format options can also be specified in metadata. For example:
#'
#' \preformatted{
#' output:
#'   pdf_document:
#'     toc: true
#'     highlight: zenburn
#' }
#'
#' Multiple formats can be specified in metadata. If no \code{output_format}
#' is passed to \code{render} then the first one defined will be used:
#'
#' \preformatted{
#' output:
#'   pdf_document:
#'     toc: true
#'     highlight: zenburn
#'   html_document:
#'     toc: true
#'     theme: united
#' }
#'
#' Formats specified in metadata can be any one of the built in formats (e.g.
#' \code{\link{html_document}}, \code{\link{pdf_document}}) or a format defined
#' in another package (e.g. \code{pkg::custom_format}).
#'
#' If there is no format defined in the YAML then
#' \code{\link{html_document}} will be used.
#' @section R Markdown:
#' R Markdown supports all of the base pandoc markdown features as well as some
#' optional features for compatibility with GitHub Flavored Markdown (which
#' previous versions of R Markdown were based on). See
#' \code{\link{rmarkdown_format}} for details.
#' @seealso
#' \link[knitr:knit]{knit}, \link{output_format},
#' \url{https://pandoc.org}
#' @inheritParams default_output_format
#' @param input The input file to be rendered. This can be an R script (.R),
#' an R Markdown document (.Rmd), or a plain markdown document.
#' @param output_format The R Markdown output format to convert to. The option
#' \code{"all"} will render all formats defined within the file. The option can
#' be the name of a format (e.g. \code{"html_document"}) and that will render
#' the document to that single format. One can also use a vector of format
#' names to render to multiple formats. Alternatively, you can pass an output
#' format object (e.g. \code{html_document()}). If using \code{NULL} then the
#' output format is the first one defined in the YAML frontmatter in the input
#' file (this defaults to HTML if no format is specified there).
#' If you pass an output format object to \code{output_format}, the options
#' specified in the YAML header or \code{_output.yml} will be ignored and you
#' must explicitly set all the options you want when you construct the object.
#' If you pass a string, the output format will use the output parameters in
#' the YAML header or \code{_output.yml}.
#' @param output_file The name of the output file. If using \code{NULL} then the
#' output filename will be based on filename for the input file. If a filename
#' is provided, a path to the output file can also be provided. Note that the
#' \code{output_dir} option allows for specifying the output file path as well,
#' however, if also specifying the path, the directory must exist. If
#' \code{output_file} is specified but does not have a file extension, an
#' extension will be automatically added according to the output format. To
#' avoid the automatic file extension, put the \code{output_file} value in
#' \code{\link{I}()}, e.g., \code{I('my-output')}.
#' @param output_dir The output directory for the rendered \code{output_file}.
#' This allows for a choice of an alternate directory to which the output file
#' should be written (the default output directory of that of the input file).
#' If a path is provided with a filename in \code{output_file} the directory
#' specified here will take precedence. Please note that any directory path
#' provided will create any necessary directories if they do not exist.
#' @param output_options List of output options that can override the options
#' specified in metadata (e.g. could be used to force \code{self_contained} or
#' \code{mathjax = "local"}). Note that this is only valid when the output
#' format is read from metadata (i.e. not a custom format object passed to
#' \code{output_format}).
#' @param intermediates_dir Intermediate files directory. If a path is specified
#' then intermediate files will be written to that path. If \code{NULL},
#' intermediate files are written to the same directory as the input file.
#' @param knit_root_dir The working directory in which to knit the document;
#' uses knitr's \code{root.dir} knit option. If \code{NULL} then the behavior
#' will follow the knitr default, which is to use the parent directory of the
#' document.
#' @param runtime The runtime target for rendering. The \code{static} option
#' produces output intended for static files; \code{shiny} produces output
#' suitable for use in a Shiny document (see \code{\link{run}}). The default,
#' \code{auto}, allows the \code{runtime} target specified in the YAML metadata
#' to take precedence, and renders for a \code{static} runtime target otherwise.
#' @param clean Using \code{TRUE} will clean intermediate files that are created
#' during rendering.
#' @param params A list of named parameters that override custom params
#' specified within the YAML front-matter (e.g. specifying a dataset to read or
#' a date range to confine output to). Pass \code{"ask"} to start an
#' application that helps guide parameter configuration.
#' @param knit_meta (This option is reserved for expert use.) Metadata
#' generated by \pkg{knitr}.
#' @param envir The environment in which the code chunks are to be evaluated
#' during knitting (can use \code{\link{new.env}()} to guarantee an empty new
#' environment).
#' @param run_pandoc An option for whether to run pandoc to convert Markdown
#' output.
#' @param quiet An option to suppress printing during rendering from knitr,
#'   pandoc command line and others. To only suppress printing of the last
#'   "Output created: " message, you can set \code{rmarkdown.render.message} to
#'   \code{FALSE}
#' @param encoding Ignored. The encoding is always assumed to be UTF-8.
#' @return
#'   When \code{run_pandoc = TRUE}, the compiled document is written into
#'   the output file, and the path of the output file is returned. When
#'   \code{run_pandoc = FALSE}, the path of the Markdown output file, with
#'   attributes \code{knit_meta} (the \pkg{knitr} meta data collected from code
#'   chunks) and \code{intermediates} (the intermediate files/directories
#'   generated by \code{render()}).
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' # Render the default (first) format defined in the file
#' render("input.Rmd")
#'
#' # Render all formats defined in the file
#' render("input.Rmd", "all")
#'
#' # Render a single format, using parameters for \code{html_document} from
#' # the YAML header parameters.
#' render("input.Rmd", "html_document")
#'
#' # Render a single format, ignoring parameters for \code{html_document} in
#' # the YAML header. Any parameters not passed as arguments to
#' # \code{html_document()} will be assigned to their default values, regardless
#' # of anything in the YAML header
#' render("input.Rmd", html_document(toc = TRUE, toc_depth = 2))
#'
#' # Render multiple formats
#' render("input.Rmd", c("html_document", "pdf_document"))
#' }
#' @export
render <- function(input,
                   output_format = NULL,
                   output_file = NULL,
                   output_dir = NULL,
                   output_options = NULL,
                   output_yaml = NULL,
                   intermediates_dir = NULL,
                   knit_root_dir = NULL,
                   runtime =  c("auto", "static", "shiny", "shinyrmd", "shiny_prerendered"),
                   clean = TRUE,
                   params = NULL,
                   knit_meta = NULL,
                   envir = parent.frame(),
                   run_pandoc = TRUE,
                   quiet = FALSE,
                   encoding = "UTF-8") {

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
    output_format <- enumerate_output_formats(input)
    if (is.null(output_format))
      output_format <- "html_document"
  }

  # check for a list of output formats -- if there is more than one
  # then recursively call this function with each format by name
  if (is.character(output_format) && length(output_format) > 1) {
    outputs <- character()
    if (length(output_file) == 1) output_file <- rep(output_file, length(output_format))
    for (i in seq_along(output_format)) {
      # the output_file argument is intentionally ignored (we can't give
      # the same name to each rendered output); copy the rest by name
      output <- render(input = input,
                       output_format = output_format[i],
                       output_file = output_file[i],
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
                       quiet = quiet)
      outputs <- c(outputs, output)
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
  requires_knit <- tolower(xfun::file_ext(input)) %in% c("r", "rmd", "rmarkdown", "qmd")

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
      stop2("The name of the input file cannot contain the special shell ",
           "characters: ", .shell_chars_regex, " (attempted to copy to a ",
           "version without those characters '", input_no_shell_chars, "' ",
           "however that file already exists)")
    }
    file.copy(input, input_no_shell_chars, overwrite = TRUE)
    intermediates <- c(intermediates, input_no_shell_chars)
    input <- input_no_shell_chars

    # if an intermediates directory wasn't explicit before, make it explicit now
    if (is.null(intermediates_dir)) {
      intermediates_dir <-
        dirname(normalize_path(input_no_shell_chars))
    }
  }

  # never use the original input directory as the intermediate directory,
  # otherwise external resources discovered will be deleted as intermediate
  # files later (because they are copied to the "intermediate" dir)
  if (!is.null(intermediates_dir) &&
      same_path(intermediates_dir, dirname(original_input)))
    intermediates_dir <- NULL

  # force evaluation of knitr root dir before we change directory context
  force(knit_root_dir)

  # execute within the input file's directory
  oldwd <- setwd(dirname(abs_path(input)))
  on.exit(setwd(oldwd), add = TRUE)

  # reset the name of the input file to be relative and generate the name of
  # the intermediate knitted file. The extension can be set as an option mainly for blogdown
  # as `.md~` will be ignored.
  input <- basename(input)
  knit_input <- input
  knit_output <- intermediates_loc(
    file_with_meta_ext(input, "knit", getOption("rmarkdown.knit.ext", "md"))
  )
  intermediates <- c(intermediates, knit_output)

  # track whether this was straight markdown input (to prevent keep_md later)
  md_input <- identical(tolower(xfun::file_ext(input)), "md")

  # if this is an R script then spin it first
  if (identical(tolower(xfun::file_ext(input)), "r")) {
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
    # append default metadata unless the field exists in YAML
    meta1 <- yaml_front_matter(knit_input)
    meta2 <- list(
      title = input, author = Sys.info()[["user"]],
      date = as.character(Sys.Date())
    )
    for (i in names(meta2)) if (!is.null(meta1[[i]])) meta2[[i]] <- NULL
    if (length(meta2)) {
      input_lines <- read_utf8(knit_input)
      write_utf8(c(input_lines, '\n\n---', yaml::as.yaml(meta2), '---'), knit_input)
    }
  }

  # read the input file
  input_lines <- read_utf8(knit_input)

  # read the yaml front matter
  front_matter <- parse_yaml_front_matter(input_lines)

  # metadata to be attached to the returned value of render() as an attribute
  old_output_metadata <- output_metadata$get()
  on.exit(output_metadata$restore(old_output_metadata), add = TRUE)
  output_metadata$restore(as.list(front_matter[['rmd_output_metadata']]))

  # if this is shiny_prerendered then modify the output format to
  # be single-page and to output dependencies to the shiny.dep file
  shiny_prerendered_dependencies <- list()
  if (requires_knit && is_shiny_prerendered(front_matter$runtime,
                                            front_matter$server)) {

    # require shiny for the knit
    if (requireNamespace("shiny")) {
      if (!"package:shiny" %in% search())
        attachNamespace("shiny")
    }
    else
      stop("The shiny package is required for shiny documents")

    # source global.R if it exists
    global_r <- file.path.ci(".", "global.R")
    if (file.exists(global_r)) {
      source(global_r, local = envir)
    }

    # force various output options
    output_options$self_contained <- FALSE
    output_options$dependency_resolver <- function(deps) {
      shiny_prerendered_dependencies <<- list(
        deps = deps,
        packages = get_loaded_packages()
      )
      list()
    }
  }

  # if we haven't been passed a fully formed output format then
  # resolve it by looking at the yaml
  if (!is_output_format(output_format)) {
    output_format <- output_format_from_yaml_front_matter(input_lines,
                                                          output_options,
                                                          output_format,
                                                          output_yaml,
                                                          output_file)
    output_format <- create_output_format(output_format$name,
                                          output_format$options)
  }
  pandoc_to <- output_format$pandoc$to

  # generate outpout file based on input filename
  output_auto <- pandoc_output_file(input, output_format$pandoc)
  if (is.null(output_file) || is.na(output_file)) output_file <- output_auto else {
    if (!inherits(output_file, "AsIs") && xfun::file_ext(output_file) == "")
      output_file <- paste(output_file, xfun::file_ext(output_auto), sep = ".")
  }

  # if an output_dir was specified then concatenate it with the output file
  if (!is.null(output_dir)) {
    output_file <- file.path(output_dir, basename(output_file))
  }
  output_dir <- dirname(output_file)

  # Stop the render process early if the output directory does not exist
  if (!dir_exists(output_dir)) {
    stop2("The directory '", output_dir, "' does not exist.")
  }

  # use output filename based files dir
  files_dir_slash <- file.path(output_dir, knitr_files_dir(basename(output_file)))
  files_dir <- pandoc_path_arg(files_dir_slash)

  # default to no cache_dir (may be generated by the knit)
  cache_dir <- NULL

  # call any intermediate files generator, if we have an intermediates directory
  # (do this before knitting in case the knit requires intermediates)
  if (!is.null(intermediates_dir) &&
      !is.null(output_format$intermediates_generator)) {
    intermediates <- c(intermediates,
                       output_format$intermediates_generator(original_input,
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
    if (is_shiny_prerendered(front_matter$runtime, front_matter$server)) {
      runtime <- "shiny_prerendered"
    } else {
      runtime <- front_matter$runtime %||% "static"
    }
  }


  # set df_print
  context <- render_context()
  context$df_print <- resolve_df_print(output_format$df_print)

  # make the front_matter available as 'metadata' within the knit environment
  # (unless it is already defined there, in which case we emit a warning)
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
  env$metadata <- front_matter

  # call any pre_knit handler
  if (!is.null(output_format$pre_knit)) {
    if ('...' %in% names(formals(output_format$pre_knit))) {
      output_format$pre_knit(input = original_input, metadata = front_matter)
    } else {
      warning("The 'pre_knit' function of the output format should have a '...' argument.")
      output_format$pre_knit(input = original_input)
    }
  }

  # function used to call post_knit handler
  call_post_knit_handler <- function() {
    if (!is.null(output_format$post_knit)) {
      post_knit_extra_args <- output_format$post_knit(front_matter,
                                                      knit_input,
                                                      runtime,
                                                      encoding = 'UTF-8')
    } else {
      post_knit_extra_args <- NULL
    }
    c(output_format$pandoc$args, post_knit_extra_args)
  }

  # determine our id-prefix (add one if necessary for runtime: shiny)
  id_prefix <- id_prefix_from_args(output_format$pandoc$args)
  if (!nzchar(id_prefix) && is_shiny(runtime, front_matter[["server"]])) {
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

    # specify that htmltools::htmlPreserve() should use the Pandoc raw attribute
    # by default (e.g. ```{=html}) rather than preservation tokens when pandoc
    # >= v2.0.
    if (pandoc2.0() && is.null(prev <- getOption("htmltools.preserve.raw"))) {
      options(htmltools.preserve.raw = TRUE)
      on.exit(options(htmltools.preserve.raw = prev), add = TRUE)
    }

    # run render on_exit (run after the knit hooks are saved so that
    # any hook restoration can take precedence)
    if (is.function(output_format$on_exit))
      on.exit(output_format$on_exit(), add = TRUE)

    # default rendering and chunk options
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)
    # the retina option does not make sense to non-HTML output formats
    if (!grepl('[.]html$', output_file)) knitr::opts_chunk$set(fig.retina = NULL)

    # store info about the final output format in opts_knit
    knitr::opts_knit$set(
      rmarkdown.pandoc.from = output_format$pandoc$from,
      rmarkdown.pandoc.to = pandoc_to,
      rmarkdown.pandoc.args = output_format$pandoc$args,
      rmarkdown.pandoc.id_prefix = id_prefix,
      rmarkdown.keep_md = output_format$keep_md,
      rmarkdown.df_print = output_format$df_print,
      rmarkdown.version = 2,
      rmarkdown.runtime = runtime
    )

    # read root directory from argument (has precedence) or front matter
    root_dir <- knit_root_dir
    if (is.null(root_dir))
      root_dir <- front_matter$knit_root_dir
    if (!is.null(root_dir))
      knitr::opts_knit$set(root.dir = root_dir)

    # use filename based figure and cache directories
    base_pandoc_to <- gsub('[-+].*', '', pandoc_to)
    if (base_pandoc_to == 'html4') base_pandoc_to <- 'html'
    knitr::opts_chunk$set(fig.path = paste0(
      pandoc_path_arg(files_dir_slash, backslash = FALSE),
      "/figure-", base_pandoc_to, "/"
    ))
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
      knitr::opts_hooks$set(label = shiny_prerendered_option_hook(original_input))
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

          # check list of previously evaluated global chunks
          code_string <- one_string(code)
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
    if (!is.null(front_matter$params)) {

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
          stop2("params object already exists in knit environment ",
               "so can't be overwritten by render params")
        }
      }

      # make the params available in the knit environment
      assign("params", params, envir = envir)
      lockBinding("params", envir)
      on.exit({
        if (exists("params", envir = envir, inherits = FALSE)) {
            do.call("unlockBinding", list("params", envir))
          if (hasParams)
            assign("params", envirParams, envir = envir)
          else
            remove("params", envir = envir)
        }
      }, add = TRUE)
    }

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
                         quiet = quiet)

    perf_timer_stop("knitr")

    front_matter <- yaml_front_matter(input)

    # call post_knit handler
    output_format$pandoc$args <- call_post_knit_handler()

    # pull any R Markdown warnings from knit_meta and emit
    rmd_warnings <- knit_meta_reset(class = "rmd_warning")
    for (rmd_warning in rmd_warnings) {
      message("Warning: ", rmd_warning)
    }

    # pull out shiny_prerendered_contexts and append them as script tags
    shiny_prerendered_append_contexts(runtime, input)

    # collect remaining knit_meta
    knit_meta <- knit_meta_reset()

  } else {
    output_format$pandoc$args <- call_post_knit_handler()
  }

  # if this isn't html and there are html dependencies then flag an error
  if (!(is_pandoc_to_html(output_format$pandoc) ||
        identical(tolower(xfun::file_ext(output_file)), "html")))  {
    if (has_html_dependencies(knit_meta)) {
      if (!isTRUE(front_matter$always_allow_html)) {
        stop2("Functions that produce HTML output found in document targeting ",
             pandoc_to, " output.\nPlease change the output type ",
             "of this document to HTML.\n",
             "If your aiming to have some HTML widgets shown in non-HTML format as a screenshot,\n",
             "please install webshot or webshot2 R package for knitr to do the screenshot, and configure it by looking at its documentation.\n",
             "Alternatively, you can allow HTML output in non-HTML formats\n",
             "by adding this option to the YAML front-matter of\nyour rmarkdown file:\n\n",
             "  always_allow_html: true\n\n",
             "Note however that the HTML output will not be visible in non-HTML formats.\n\n"
        )
      }
    }
    if (!identical(runtime, "static")) {
      stop2("Runtime '", runtime, "' is not supported for ",
           pandoc_to, " output.\nPlease change the output type ",
           "of this document to HTML.")
    }
  }

  # clean the files_dir if we've either been asking to clean supporting files or
  # the knitr cache is active; clean the figure-* dir instead of the whole
  # files_dir if other subdirs are generated by another format and still needed:
  # https://github.com/rstudio/rmarkdown/issues/1472 and also #1503
  intermediates_fig <- if (output_format$clean_supporting && !dir_exists(cache_dir)) {
    # unlink does not support / at the end of file path
    fig_path <- gsub("/$", "", knitr::opts_chunk$get('fig.path'))

    # existing figure folder(s), can be character(0)
    # if no figure is generated, clean the whole files_dir (#1664)
    files_dir_fig <- list.files(files_dir, '^figure-.+')

    if (length(files_dir_fig) < 1 || identical(files_dir_fig, basename(fig_path))) {
      files_dir
    } else {
      fig_path
    }
  }
  intermediates <- c(intermediates, intermediates_fig)

  if (run_pandoc) {

    # set env vars required during Pandoc processing
    lua_env_vars <- xfun::set_envvar(c(RMARKDOWN_LUA_SHARED = pkg_file_lua("shared.lua")))
    on.exit(xfun::set_envvar(lua_env_vars), add = TRUE)

    perf_timer_start("pre-processor")

    if (has_dependencies(knit_meta, "output_format_dependency")) {
        output_format <- merge_output_format_dependencies(output_format, knit_meta)
    }
    # call any pre_processor
    if (!is.null(output_format$pre_processor)) {
      extra_args <- output_format$pre_processor(front_matter,
                                                input,
                                                runtime,
                                                knit_meta,
                                                files_dir,
                                                output_dir)
      output_format$pandoc$args <- c(output_format$pandoc$args, extra_args)
    }

    # write shiny_prerendered_dependencies if we have them
    if (is_shiny_prerendered(runtime)) {
      shiny_prerendered_append_dependencies(input,
                                            shiny_prerendered_dependencies,
                                            files_dir,
                                            output_dir)
      # Include special comment in header for correc insertion of HTML dependencies
      # during shiny_prerendered process (See `shiny_prerendered_html()`).
      # This should be the last header-includes to be set.
      # Context in https://github.com/rstudio/rmarkdown/pull/2249
      output_format$pandoc$args <- c(output_format$pandoc$args,
                                     pandoc_include_args(in_header = pkg_file("rmd/h/shiny-header.html")))
    }

    perf_timer_stop("pre-processor")

    need_bibtex <- grepl('[.](pdf|tex)$', output_file) &&
      any(c('--natbib', '--biblatex') %in% output_format$pandoc$args)

    perf_timer_start("pandoc")

    convert <- function(output, citeproc = FALSE) {

      # temporarily move figures and bib files to the intermediate dir if
      # specified: https://github.com/rstudio/rmarkdown/issues/500
      if (!is.null(intermediates_dir)) {
        figures_dir <- gsub('/$', '', knitr::opts_chunk$get("fig.path"))
        files <- list.files(figures_dir, full.names = TRUE, recursive = TRUE)
        # https://github.com/rstudio/rmarkdown/issues/1358
        if (citeproc) files <- c(files, front_matter[['bibliography']])
        for (f in files) {
          intermediates <<- c(intermediates, copy_file_with_dir(f, intermediates_dir))
        }
      }

      # ensure we expand paths (for Windows where leading `~/` does
      # not get expanded by pandoc)
      input  <- path.expand(input)
      output <- path.expand(output)

      pandoc_args <- output_format$pandoc$args

      # if Lua filters are provided, add the command line switch
      if (!is.null(lua_filters <- output_format$pandoc$lua_filters)) {
        lua_filters <- pandoc_lua_filter_args(lua_filters)
      }
      pandoc_args <- c(lua_filters, pandoc_args)

      # in case the output format turns on the --file-scope flag, run its
      # file_scope function to split the input into multiple files
      input_files <- input
      if (is.function(output_format$file_scope)) {
        input_files <- file_scope_split(input, output_format$file_scope)
        # ignore if input_files has not really been splitted
        if (length(input_files) > 1) {
          # add the --file-scope option
          pandoc_args <- c(pandoc_args, "--file-scope")
          # cleanup the split files after render
          on.exit(unlink(input_files), add = TRUE)
        }
      }

      # use the convert function from the output format if provided
      if (!is.function(convert_fun <- output_format$pandoc$convert_fun))
        convert_fun <- pandoc_convert
      convert_it <- function(output) convert_fun(
        output = output, input_files, pandoc_to, output_format$pandoc$from,
        citeproc, pandoc_args, !quiet
      )

      # if we don't detect any invalid shell characters in the
      # target path, then just call pandoc directly
      if (!grepl(.shell_chars_regex, output) && !grepl(.shell_chars_regex, input)) {
        return(convert_it(output))
      }

      # render to temporary file (preserve extension)
      # this also ensures we don't pass a file path with invalid
      # characters to our pandoc invocation
      ext <- xfun::file_ext(output)
      if (ext != '') ext <- paste0('.', ext)

      # render to a path in the current working directory
      # (avoid passing invalid characters to shell)
      pandoc_output_tmp <- basename(tempfile("pandoc", getwd(), ext))

      # clean up temporary file on exit
      on.exit(unlink(pandoc_output_tmp), add = TRUE)

      # call pandoc to render file
      status <- convert_it(pandoc_output_tmp)

      # construct output path (when passed only a file name to '--output',
      # pandoc seems to render in the same directory as the input file)
      pandoc_output_tmp_path <- file.path(dirname(input), pandoc_output_tmp)

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
    # determine whether we need to run citeproc (based on whether we have
    # references in the input)
    run_citeproc <- citeproc_required(front_matter, input_lines)
    # if the output format is LaTeX, first convert .md to .tex, and then convert
    # .tex to .pdf via latexmk() if PDF output is requested (in rmarkdown <=
    # v1.8, we used to call Pandoc to convert .md to .tex and .pdf separately)
    if (output_format$pandoc$keep_tex || knitr::is_latex_output()) {
      # do not use pandoc-citeproc if needs to build bibliography
      convert(texfile, run_citeproc && !need_bibtex)
      # patch the .tex output generated from the default Pandoc LaTeX template
      if (!("--template" %in% output_format$pandoc$args)) patch_tex_output(texfile)
      fix_horiz_rule(texfile)
      # unless the output file has the extension .tex, we assume it is PDF
      if (!grepl('[.]tex$', output_file)) {
        latexmk(texfile, output_format$pandoc$latex_engine, '--biblatex' %in% output_format$pandoc$args)
        file.rename(file_with_ext(texfile, "pdf"), output_file)
        # clean up the tex file if necessary
        if (!output_format$pandoc$keep_tex) {
          texfile <- normalize_path(texfile)
          on.exit(unlink(texfile), add = TRUE)
        }
      }
    } else {
      convert(output_file, run_citeproc)
    }

    # pandoc writes the output alongside the input, so if we rendered from an
    # intermediate directory, move the output file
    if (!is.null(intermediates_dir)) {
      intermediate_output <- file.path(intermediates_dir, basename(output_file))
      if (file.exists(intermediate_output)) {
        move_dir(intermediate_output, output_file)
      }
    }

    perf_timer_stop("pandoc")

    perf_timer_start("post-processor")

    # if there is a post-processor then call it
    if (!is.null(output_format$post_processor))
      output_file <- output_format$post_processor(front_matter,
                                                  input,
                                                  output_file,
                                                  clean,
                                                  !quiet)

    if (!quiet && getOption('rmarkdown.render.message', TRUE)) {
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
    output_file <- abs_path(output_file)
    # attach the metadata specified as rmd_output_metadata in YAML
    if (length(output_meta <- output_metadata$get()))
      attr(output_file, 'rmd_output_metadata') <- output_meta
    invisible(output_file)
  } else {
    # make sure the markdown output and fig dir are not cleaned up
    intermediates <- setdiff(intermediates, c(input, intermediates_fig))
    # did not run pandoc; returns the markdown output with attributes of the
    # knitr meta data and intermediate files
    invisible(structure(input,
              knit_meta = knit_meta,
              files_dir = files_dir,
              intermediates_dir = intermediates_fig,
              intermediates = intermediates))
  }
}


#' Render supporting files for an input document
#'
#' Render (copy) required supporting files for an input document to the
#' \code{_files} directory that is associated with the document.
#'
#' @param from The directory from which the files should be copied.
#' @param files_dir The directory that will receive the copied files.
#' @param rename_to An option to rename the source directory after the copy
#' operation is complete.
#' @return The relative path to the supporting files. This path is suitable
#' for inclusion in HTML\code{href} and \code{src} attributes.
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
      stop2('Invalid value for df_print (valid values are ',
           paste(valid_methods, collapse = ", "))
  }

  df_print
}


# package level globals
.globals <- new.env(parent = emptyenv())
.globals$evaluated_global_chunks <- character()
.globals$level <- 0L


#' The output metadata object
#'
#' This object provides a mechanism for users to attach metadata as an attribute
#' (named \code{rmd_output_metadata}) of the returned value of
#' \code{\link{render}()}. The initial value of the metadata comes from in the
#' \code{rmd_output_metadata} field of the YAML frontmatter of an R Markdown
#' document. The metadata can be queried via the
#' \code{output_metadata$get()} method, and modified via the
#' \code{output_metadata$set()} method.
#' @format NULL
#' @usage NULL
#' @keywords NULL
#' @export
output_metadata = knitr:::new_defaults()

file_scope_split <- function(input, fun) {
  inputs <- if (length(formals(fun)) == 1L) {
    fun(input) # for the backward compatibility
  } else {
    fun(input, NULL) # the second argument implies current file scope
  }

  # file_scope_fun should split the input file in several
  # do nothing if not and return input file unsplited
  if (length(inputs) <= 1) return(input)

  # write the split content into *.split.md files
  input_files <- lapply(inputs, function(f) {
    file <- file_with_meta_ext(f$name, "split", "md")
    file <- file.path(dirname(input), file)
    write_utf8(f$content, file)
    file
  })

  unlist(input_files)
}
