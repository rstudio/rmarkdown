
#' @export
ioslides_presentation <- function(logo = NULL,
                                  incremental = FALSE,
                                  fig_width = 7.5,
                                  fig_height = 4.5,
                                  fig_retina = 2,
                                  fig_caption = FALSE,
                                  smart = TRUE,
                                  self_contained = TRUE,
                                  widescreen = FALSE,
                                  smaller = FALSE,
                                  transition = "default",
                                  mathjax = "default",
                                  css = NULL,
                                  includes = NULL,
                                  lib_dir = NULL,
                                  data_dir = NULL,
                                  pandoc_args = NULL) {

  # base pandoc options for all output
  args <- c()

  # no email obfuscation
  args <- c(args, "--email-obfuscation", "none")

  # smart quotes
  if (smart)
    args <- c(args, "--smart")

  # self contained document
  if (self_contained) {
    validate_self_contained(mathjax)
    args <- c(args, "--self-contained")
  }

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

  # data dir
  if (!is.null(data_dir))
    args <- c(args, "--data-dir", pandoc_path_arg(data_dir))

  # template path and assets
  args <- c(args,
            "--template",
            pandoc_path_arg(rmarkdown_system_file("rmd/ioslides/default.html")))

  # custom args
  args <- c(args, pandoc_args)

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(input_lines, knit_meta, files_dir) {

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # extra args
    args <- c()

    # create the files dir if it doesn't exist
    if (!file.exists(files_dir))
      dir.create(files_dir)

    # logo
    if (!is.null(logo)) {
      logo_path <- logo
      if (!self_contained) {
        logo_path <- file.path(files_dir, "logo.png")
        file.copy(from = logo, to = logo_path)
      }
      args <- c(args, "--variable", paste("logo=",
                                          pandoc_path_arg(logo_path),
                                          sep = ""))
    }

    # resolve and inject dynamic html dependencies
    dependencies <- html_dependencies_for_document(NULL, knit_meta)
    args <- c(args, html_dependencies_to_pandoc_args(dependencies,
                                                     self_contained,
                                                     lib_dir))

    # ioslides
    ioslides_path <- rmarkdown_system_file("rmd/ioslides/ioslides-13.5.1")
    if (!self_contained)
      ioslides_path <- render_supporting_files(ioslides_path, lib_dir)
    args <- c(args, "--variable", paste("ioslides-url=",
                                        pandoc_path_arg(ioslides_path),
                                        sep=""))

    # mathjax
    args <- c(args, pandoc_mathjax_args(mathjax,
                                        "default",
                                        self_contained,
                                        lib_dir))

    # return additional args
    args
  }

  # post processor that renders our markdown using out custom lua
  # renderer and then inserts it into the main file
  post_processor <- function(input_file, output_file, verbose) {

    # setup args
    args <- c()

    # see if we have a base64 image encoder
    base64_encoder <- base64_image_encoder()

    # convert using our lua writer (write output to a temp file)
    lua_writer <- "ioslides_presentation.lua"
    on.exit(unlink(lua_writer), add = TRUE)

    # write settings to file
    settings <- c()
    add_setting <- function(name, value) {
      settings <<- c(settings, paste("local", name, "=",
                                    ifelse(value, "true", "false")))
    }
    add_setting("fig_caption", fig_caption)
    add_setting("incremental", incremental)
    add_setting("base64_images", self_contained && is.null(base64_encoder))
    add_setting("smaller", smaller)
    add_setting("smart", smart)
    add_setting("mathjax", !is.null(mathjax))
    writeLines(settings, lua_writer, useBytes = TRUE)

    # append main body of script
    file.append(lua_writer,
                rmarkdown_system_file("rmd/ioslides/ioslides_presentation.lua"))

    output_tmpfile <- tempfile("ioslides-output", fileext = ".html")
    on.exit(unlink(output_tmpfile), add = TRUE)
    pandoc_convert(input = input_file,
                   to = pandoc_path_arg(lua_writer),
                   from = from_rmarkdown(fig_caption),
                   output = output_tmpfile,
                   options = args,
                   verbose = verbose)

    # read the slides
    slides_lines <- readLines(output_tmpfile, warn = FALSE, encoding = "UTF-8")

    # base64 encode if needed
    if (self_contained && !is.null(base64_encoder))
      slides_lines <- base64_encoder(slides_lines)

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
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina),
    pandoc = pandoc_options(to = "html",
                            from = from_rmarkdown(fig_caption),
                            args = args),
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    post_processor = post_processor
  )
}

