#' Base output format for HTML-based output formats
#'
#' Creates an HTML base output format suitable for passing as the
#' \code{base_format} argument of the \code{\link{output_format}} function.
#'
#' @inheritParams html_document
#'
#' @param dependency_resolver A dependency resolver
#' @param copy_resources Copy resources
#' @param bootstrap_compatible Bootstrap compatible
#' @param allow_uptree_lib_dir Allow lib_dir not to be a descendent of the
#'   output directory.
#' @param ... Ignored
#'
#' @return HTML base output format.
#'
#' @export
html_document_base <- function(theme = NULL,
                               self_contained = TRUE,
                               lib_dir = NULL,
                               math_method = "default",
                               mathjax = "default",
                               pandoc_args = NULL,
                               template = "default",
                               dependency_resolver = NULL,
                               copy_resources = FALSE,
                               extra_dependencies = NULL,
                               css = NULL,
                               bootstrap_compatible = FALSE,
                               allow_uptree_lib_dir = FALSE,
                               ...) {

  # default for dependency_resolver
  if (is.null(dependency_resolver))
    dependency_resolver <- html_dependency_resolver

  args <- c()

  # backward compatibility for math /mathjax argument
  # math is used only when `mathjax = "default"`
  math <- mathjax_to_math(mathjax, math_method)
  # any math to list(engine = , url = )
  math <- check_math_argument(math)

  # self contained document
  if (self_contained) {
    if (copy_resources)
      stop("Local resource copying is incompatible with self-contained documents.")
    validate_self_contained(math)
    args <- c(args, self_contained_args())
  }

  # custom args
  args <- c(args, pandoc_args)

  preserved_chunks <- character()

  output_dir <- ""

  theme <- resolve_theme(theme)

  # In the bs_theme() case, we set the theme globally so that knitting code may
  # alter it before we ultimately compile it into an HTML dependency.
  old_theme <- NULL
  pre_knit <- function(input, ...) {
    if (is_bs_theme(theme)) {
      # merge css file with bslib mechanism...
      for (f in css) theme <<- bslib::bs_add_rules(theme, xfun::read_utf8(f))
      # ...and don't process CSS files further in Pandoc
      css <<- NULL

      # save old theme
      old_theme <<- bslib::bs_global_set(theme)
    }
  }
  post_knit <- function(metadata, input_file, runtime, ...) {}
  on_exit <- function() {
    # In this case, we know we've altered global state, so restore the old theme
    if (is_bs_theme(theme)) bslib::bs_global_set(old_theme)
  }

  # pre_processor
  pre_processor <- function(metadata, input_file, runtime, knit_meta,
                            files_dir, output_dir) {

    args <- c()

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <<- files_dir

    # copy supplied output_dir (for use in post-processor)
    output_dir <<- output_dir

    if (!is.null(theme)) {
      theme_arg <- if (is.list(theme)) "bootstrap" else theme
      args <- c(args, pandoc_variable_arg("theme", theme_arg))
    }

    # Process css files as Pandoc argument if not already been processed by bslib
    for (f in css) {
      if (needs_sass(f)) {
        if (!xfun::loadable("sass")) {
          stop2("Using `.sass` or `.scss` file in `css` argument requires the sass package.")
        }
        f <- sass::sass(
          sass::sass_file(f),
          # write output file to `lib_dir/sass-{sass:::sass_hash()}{[basename(f)}`
          output = sass::output_template(
            basename = xfun::sans_ext(basename(f)),
            dirname = "sass",
            path = lib_dir
          ),
          options = sass::sass_options(output_style = "compressed")
        )
        f <- normalized_relative_to(output_dir, f)
      }
      args <- c(args, "--css", pandoc_path_arg(f, backslash = FALSE))
    }

    # math support
    math_support <- add_math_support(math, template, lib_dir, output_dir)
    args <- c(args, math_support$args)
    extra_dependencies <- c(extra_dependencies, math_support$extra_dependencies)

    # resolve and inject extras, including dependencies specified by the format
    # and dependencies specified by the user (via extra_dependencies)
    format_deps <- list()
    format_deps <- append(format_deps, html_dependency_header_attrs())
    if (!is.null(theme)) {
      format_deps <- append(format_deps, list(html_dependency_jquery()))
      # theme was set globally pre-knit, and it may be modified during knit
      if (is_bs_theme(theme)) {
        theme <- bslib::bs_global_get()
      }
      bootstrap_deps <- if (is_bs_theme(theme) && is_shiny(runtime, metadata[["server"]])) {
        list(shiny_bootstrap_lib(theme))
      } else {
        bootstrap_dependencies(theme)
      }
      format_deps <- append(format_deps, htmltools::resolveDependencies(bootstrap_deps))
    }
    else if (isTRUE(bootstrap_compatible) && is_shiny(runtime, metadata[["server"]])) {
      # If we can add bootstrap for Shiny, do it
      format_deps <- append(format_deps, bootstrap_dependencies("bootstrap"))
    }

    # Allow the user to override default dependencies by injecting alternate
    # dependencies with the same name into extra_dependencies.
    if (length(format_deps) > 0  && length(extra_dependencies) > 0) {
      names(format_deps) <- lapply(format_deps, function(x) x$name)
      names(extra_dependencies) <- lapply(extra_dependencies, function(x) x$name)
      format_deps <- format_deps[setdiff(names(format_deps),
                                         names(extra_dependencies))]
    }
    format_deps <- append(format_deps, extra_dependencies)

    extras <- html_extras_for_document(knit_meta, runtime, dependency_resolver,
                                       format_deps)
    args <- c(args, pandoc_html_extras_args(extras, self_contained, lib_dir,
                                            output_dir, allow_uptree_lib_dir))

    preserved_chunks <<- extract_preserve_chunks(input_file)

    args
  }

  intermediates_generator <- function(original_input, intermediates_dir) {
    # copy intermediates; skip web resources if not self contained (pandoc can
    # create references to web resources without the file present)
    copy_render_intermediates(original_input, intermediates_dir, !self_contained)
  }

  post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    # Special KaTeX math support
    if (identical(math_method, "r-katex") && xfun::pkg_available("katex", "1.4.0")) {
      katex::render_math_in_html(output_file, output = output_file)
    }

    # Other processing ----

    # read the output file
    output_str <- read_utf8(output_file)

    # TODO: remove this temporary fix after the syntax highlighting problem is
    # fixed in Pandoc https://github.com/rstudio/bookdown/issues/1157
    s1 <- '<span class="sc">|</span><span class="er">&gt;</span>'
    s2 <- '<span class="ot">=</span><span class="er">&gt;</span>'

    # if there are no preserved chunks to restore and no resource to copy then no
    # post-processing is necessary
    if ((length(preserved_chunks) == 0 && !isTRUE(copy_resources) && self_contained) &&
        !length(c(grep(s1, output_str, fixed = TRUE), grep(s2, output_str, fixed = TRUE))))
      return(output_file)

    # if we preserved chunks, restore them
    if (length(preserved_chunks) > 0) {
      # Pandoc adds an empty <p></p> around the IDs of preserved chunks, and we
      # need to remove these empty tags, otherwise we may have invalid HTML like
      # <p><div>...</div></p>. For the reason of the second gsub(), see
      # https://github.com/rstudio/rmarkdown/issues/133.
      for (i in names(preserved_chunks)) {
        output_str <- gsub(paste0("<p>", i, "</p>"), i, output_str,
                           fixed = TRUE, useBytes = TRUE)
        output_str <- gsub(paste0(' id="[^"]*?', i, '[^"]*?" '), ' ', output_str,
                           useBytes = TRUE)
      }
      output_str <- restorePreserveChunks(output_str, preserved_chunks)
    }

    if (copy_resources) {
      # The copy_resources flag copies all the resources referenced in the
      # document to its supporting files directory, and rewrites the document to
      # use the copies from that directory.
      output_str <- copy_html_resources(one_string(output_str), lib_dir, output_dir)
    } else if (!self_contained) {
      # if we're not self-contained, find absolute references to the output
      # directory and replace them with relative ones
      image_relative <- function(img_src, src) {
        in_file <- utils::URLdecode(src)
        # do not process paths that are already relative
        if (grepl('^[.][.]', in_file)) return(img_src)
        if (length(in_file) && file.exists(in_file)) {
          img_src <- sub(
            src, utils::URLencode(normalized_relative_to(output_dir, in_file)),
            img_src, fixed = TRUE)
        }
        img_src
      }
      output_str <- process_images(output_str, image_relative)
    }

    # fix the issue mentioned in TODO above
    output_str <- gsub(s1, '<span class="sc">|&gt;</span>', output_str, fixed = TRUE)
    output_str <- gsub(s2, '<span class="ot">=&gt;</span>', output_str, fixed = TRUE)

    write_utf8(output_str, output_file)

    output_file
  }

  if (!is.null(theme)) {
    bs3 <- identical("3", theme_version(theme))
    args <- c(args, pandoc_variable_arg("bs3", bs3))
  }

  output_format(
    knitr = NULL,
    pandoc = pandoc_options(
      to = "html", from = NULL, args = args,
      lua_filters = pkg_file_lua(c(
        "pagebreak.lua", "latex-div.lua",
        if (pandoc_available("3.2.1")) "table-classes.lua"
      ))
    ),
    keep_md = FALSE,
    clean_supporting = FALSE,
    pre_knit = pre_knit,
    post_knit = post_knit,
    on_exit = on_exit,
    pre_processor = pre_processor,
    intermediates_generator = intermediates_generator,
    post_processor = post_processor
  )
}

extract_preserve_chunks <- function(input_file, extract = extractPreserveChunks) {
  # Don't try to modify the input file if it's not .md, otherwise the original
  # input could be corrupted (#2534). In theory, preserved chunks should only
  # exist in the intermediate .md file from knit(). If the .md file is not
  # intermediate but original, this processing should be harmless.
  if (!xfun::file_ext(input_file) %in% c('md', 'markdown')) return()
  input_str <- one_string(read_utf8(input_file))
  preserve <- extract(input_str)
  if (!identical(preserve$value, input_str)) write_utf8(preserve$value, input_file)
  preserve$chunks
}


# Math support ------------------------------------------------------------

default_mathjax <- function() {
  paste0("https://mathjax.rstudio.com/latest/", mathjax_config())
}

mathjax_config <- function() {
  "MathJax.js?config=TeX-AMS-MML_HTMLorMML"
}

add_math_support <- function(math, template, files_dir, output_dir) {

  # check math argument should be already or NULL
  # (list(engine = "", url = "")

  extras <- NULL

  if (is.null(math)) return(NULL)

  # handle different engines

  # Special handling: KaTeX R package
  if (identical(math$engine, "r-katex")) {
    if (xfun::pkg_available("katex", "1.4.0")) {
      # We need to tell pandoc to process the equation,
      # setting no math argument will make Pandoc throw a warning
      # If used with a template contained `$math$`, JS and CSS will be inserted
      # TODO: patch template to remove the math variable when needed.
      return(list(args = pandoc_math_args("katex")))
    }
    stop2("katex R package (>= 1.4.0) is required for server-side rendering.\n",
          "Install the package or change `math_method`.")
  }

  # Default
  if (identical(math$engine, "default")) math$engine <- "mathjax"

  # Check supported
  if (!math$engine %in% c(pandoc_math_engines())) {
    stop2(sprintf("`math_method='%s'` is not supported.", math$engine))
  }

  # support only mathjax for Pandoc before 2.0+
  if (!pandoc2.0() && !identical(math$engine, "mathjax")) {
    stop2("only `math_method = 'mathjax'` is supported with earlier version than Pandoc 2.0 ")
  }

  # change default for url for webtex to use SVG
  # Pandoc still uses PNG
  if (identical(math$engine, "webtex")) {
    math$url <- math$url %||% "https://latex.codecogs.com/svg.image?"
  }

  # No special handling needed for most engine
  if (math$engine %in% setdiff(pandoc_math_engines(), c("mathjax", "katex"))) {
    return(list(args = pandoc_math_args(math$engine, math$url)))
  }

  # MATHJAX OR KATEX needs special handling

  if (identical(math$engine, "katex")) {
    if (identical(template, "default")) {
      args <- pandoc_math_args(math$engine)
      extras <- list(html_dependency_katex(math$url))
    } else {
      args <- c(pandoc_math_args(math$engine, math$url))
    }
  } else if (identical(math$engine, "mathjax")) {
    if (identical(math$url, "local")) {
      # local supported for mathjax only
      mathjax_path <- render_supporting_files(
        pandoc_mathjax_local_path(),
        files_dir,
        "mathjax-local")
      math$url <- paste(normalized_relative_to(output_dir, mathjax_path), "/",
                        mathjax_config(), sep = "")
    }
    if (identical(template, "default")) {
      args <- c(
        pandoc_math_args(math$engine),
        pandoc_variable_arg("mathjax-url", math$url %||% default_mathjax())
      )
    } else {
      args <- c(pandoc_math_args(math$engine, math$url))
    }
  }

  list(args =  args, extra_dependencies = extras)
}

check_math_argument <- function(math) {
  url <- NULL
  engine <- NULL
  # math is deactivated
  if (is.null(math)) return(NULL)
  # otherwise
  if (is.list(math) && !is.null(names(math))) {
    # can be a named list
    engine <- if (is.character(math$engine)) math$engine
    url <- math$url
  } else if (length(math) == 1L && is.character(math[[1L]])) {
    # can be a string
    engine <- math[[1L]]
  }

  # if no engine found, incorrect value must have been provided
  if (is.null(engine)) {
    stop2("'math' can be the engine name (a string) or a list with engine and optionally the url to use.")
  }

  list(engine = engine, url = url)
}

mathjax_to_math <- function(mathjax, math) {
  if (is.null(mathjax)) {
    # deactivate math
    return(NULL)
  } else if (identical(mathjax, "default")) {
    # default to other argument now
    return(math)
  } else if (identical(mathjax, "local")) {
    # use local mathajx
    return(list(engine = "mathjax", url = "local"))
  } else if (is.logical(mathjax)) {
    # let mathjax = FALSE deactivate
    return(if(isTRUE(mathjax)) math)
  } else if (is.character(mathjax)) {
    # any other string should be a mathjax url
    return(list(engine = "mathjax", url = mathjax))
  }
  # just return math for any other value
  math
}
