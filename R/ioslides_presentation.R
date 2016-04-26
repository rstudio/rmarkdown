
#' @export
ioslides_presentation <- function(logo = NULL,
                                  incremental = FALSE,
                                  fig_width = 7.5,
                                  fig_height = 4.5,
                                  fig_retina = 2,
                                  fig_caption = TRUE,
                                  dev = 'png',
                                  smart = TRUE,
                                  self_contained = TRUE,
                                  widescreen = FALSE,
                                  smaller = FALSE,
                                  transition = "default",
                                  mathjax = "default",
                                  analytics = NULL,
                                  template = NULL,
                                  css = NULL,
                                  includes = NULL,
                                  keep_md = FALSE,
                                  lib_dir = NULL,
                                  md_extensions = NULL,
                                  pandoc_args = NULL,
                                  ...) {

  # base pandoc options for all output
  args <- c()

  # widescreen
  if (widescreen)
    args <- c(args, "--variable", "widescreen");

  # transition
  if (is.numeric(transition))
    transition <- as.character(transition)
  else if (transition %in% c("default", "faster", "slower"))
    transition <- switch(transition,
                         "default" = "0.4",
                         "faster" = "0.2",
                         "slower" = "0.6")
  else
    stop('transition must be "default", "faster", "slower" or a ',
         'numeric value (representing seconds)', call. = FALSE)
  args <- c(args, pandoc_variable_arg("transition", transition))

  # additional css
  for (css_file in css)
    args <- c(args, "--css", pandoc_path_arg(css_file))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # template path and assets
  if (!is.null(template) && file.exists(template))
    args <- c(args, "--template", template)
  else
    args <- c(args,
              "--template",
              pandoc_path_arg(rmarkdown_system_file("rmd/ioslides/default.html")))

  # analytics
  if(!is.null(analytics))
    args <- c(args, pandoc_variable_arg("analytics", analytics))

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # extra args
    args <- c()

    # create the files dir if it doesn't exist
    if (!dir_exists(files_dir))
      dir.create(files_dir)

    # logo
    if (!is.null(logo)) {
      logo_path <- logo
      if (!self_contained) {
        # use same extension as specified logo (default is png if unspecified)
        logo_ext <- tools::file_ext(logo)
        if (nchar(logo_ext) < 1)
          logo_ext <- "png"
        logo_path <- file.path(files_dir, paste("logo", logo_ext, sep = "."))
        file.copy(from = logo, to = logo_path)
        logo_path <- normalized_relative_to(output_dir, logo_path)
      } else {
        logo_path <- pandoc_path_arg(logo_path)
      }
      args <- c(args, "--variable", paste("logo=", logo_path, sep = ""))
    }

    # ioslides
    ioslides_path <- rmarkdown_system_file("rmd/ioslides/ioslides-13.5.1")
    if (!self_contained)
      ioslides_path <- normalized_relative_to(output_dir,
        render_supporting_files(ioslides_path, lib_dir))
    else
      ioslides_path <- pandoc_path_arg(ioslides_path)
    args <- c(args, "--variable", paste("ioslides-url=", ioslides_path, sep=""))

    # return additional args
    args
  }

  # post processor that renders our markdown using our custom lua
  # renderer and then inserts it into the main file
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    # setup args
    args <- c()

    # add any custom pandoc args
    args <- c(args, pandoc_args)

    # attempt to create the output writer alongside input file
    lua_writer <- file.path(dirname(input_file), "ioslides_presentation.lua")
    tryCatch({
      suppressWarnings(writeLines("", lua_writer, useBytes = TRUE))
    },
    error = function(...) {
      # The input directory may not be writable (on e.g. Shiny Server), so write
      # to the output directory in this case. We don't always do this since
      # supplying a fully qualified path to the writer can trigger a bug on some
      # Linux configurations.
      lua_writer <<- file.path(dirname(output_file),
                               "ioslides_presentation.lua")
    })
    on.exit(unlink(lua_writer), add = TRUE)

    # determine whether we need to run citeproc
    input_lines <- readLines(input_file, warn = FALSE)
    run_citeproc <- citeproc_required(metadata, input_lines)

    # write settings to file
    settings <- c()
    add_setting <- function(name, value) {
      settings <<- c(settings, paste("local", name, "=",
                                    ifelse(value, "true", "false")))
    }
    add_setting("fig_caption", fig_caption)
    add_setting("incremental", incremental)
    add_setting("smaller", smaller)
    add_setting("smart", smart)
    add_setting("mathjax", !is.null(mathjax))
    writeLines(settings, lua_writer, useBytes = TRUE)

    # append main body of script
    file.append(lua_writer,
                rmarkdown_system_file("rmd/ioslides/ioslides_presentation.lua"))

    output_tmpfile <- tempfile("ioslides-output", fileext = ".html")
    on.exit(unlink(output_tmpfile), add = TRUE)

    # on Windows, cache the current codepage and set it to 65001 (UTF-8) for the
    # duration of the Pandoc command. Without this, Pandoc fails when attempting
    # to hand UTF-8 encoded non-ASCII characters over to the custom Lua writer.
    # See https://github.com/rstudio/rmarkdown/issues/134
    if (is_windows()) {
      # 'chcp' returns e.g., "Active code page: 437"; strip characters and parse
      # the number
      codepage <- as.numeric(gsub("\\D", "", system2("chcp", stdout = TRUE)))

      if (!is.na(codepage)) {
        # if we got a valid codepage, restore it on exit
        on.exit(system2("chcp", args = codepage, stdout = TRUE), add = TRUE)

        # change to the UTF-8 codepage
        system2("chcp", args = 65001, stdout = TRUE)
      }
    }

    pandoc_convert(input = input_file,
                   to = relative_to(dirname(input_file), lua_writer),
                   from = from_rmarkdown(fig_caption),
                   output = output_tmpfile,
                   options = args,
                   citeproc = run_citeproc,
                   verbose = verbose)

    # read the slides
    slides_lines <- readLines(output_tmpfile, warn = FALSE, encoding = "UTF-8")

    # base64 encode if needed
    if (self_contained) {
      base64_encoder <- base64_image_encoder()
      slides_lines <- base64_encoder(slides_lines)
    }

    # read the output file
    output_lines <- readLines(output_file, warn = FALSE, encoding = "UTF-8")

    # substitute slides for the sentinel line
    sentinel_line <- grep("^RENDERED_SLIDES$", output_lines)
    if (length(sentinel_line) == 1) {
      preface_lines <- c(output_lines[1:sentinel_line[1]-1])
      suffix_lines <- c(output_lines[-(1:sentinel_line[1])])
      output_lines <- c(preface_lines, slides_lines, suffix_lines)
      writeLines(output_lines, output_file, useBytes = TRUE)
    } else {
      stop("Slides placeholder not found in slides HTML", call. = FALSE)
    }

    output_file
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev),
    pandoc = pandoc_options(to = "html",
                            from = from_rmarkdown(fig_caption, md_extensions),
                            args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = html_document_base(smart = smart, lib_dir = lib_dir,
                                     self_contained = self_contained,
                                     mathjax = mathjax,
                                     pandoc_args = pandoc_args,
                                     bootstrap_compatible = TRUE, ...))
}

