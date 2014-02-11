

#' Convert to an ioslides Presentation
#'
#' Convert to an ioslides Presentation
#'
#' @inheritParams beamer_presentation
#' @inheritParams html_document
#'
#' @param widescreen Display presentation with wider dimensions.
#' @param smaller Use smaller text on all slides.
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' If a \code{before_body} include is specified then it will replace
#' the standard title slide entirely.
#'
#' For more information on markdown syntax for presentations see
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing slide shows with pandoc}.
#'
#' @export
ioslides_presentation <- function(incremental = FALSE,
                                  fig_width = 7.5,
                                  fig_height = 5.5,
                                  fig_retina = 2,
                                  fig_caption = FALSE,
                                  smart = TRUE,
                                  self_contained = TRUE,
                                  widescreen = FALSE,
                                  smaller = FALSE,
                                  mathjax = "default",
                                  css = NULL,
                                  includes = NULL,
                                  data_dir = NULL,
                                  pandoc_args = NULL) {

  # interplay between arguments
  self_contained <- reconcile_self_contained(self_contained, mathjax)

  # base pandoc options for all output
  args <- c()

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  # self contained document
  if (self_contained)
    args <- c(args, "--self-contained")

  # widescreen
  if (widescreen)
    args <- c(args, "--variable", "widescreen");

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

  # build a filter we'll use for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  format_filter <- function(output_format, files_dir, input_lines) {

    # extra args
    args <- c()

    # ioslides
    ioslides_path <- rmarkdown_system_file("rmd/ioslides/ioslides-13.5.1")
    if (!self_contained)
      ioslides_path <- render_supporting_files(ioslides_path, files_dir)
    args <- c(args, "--variable", paste("ioslides-url=", ioslides_path, sep=""))

    # mathjax
    args <- c(args, pandoc_mathjax_args(mathjax,
                                        "default",
                                        self_contained,
                                        files_dir))

    # return format with ammended args
    output_format$pandoc$args <- c(output_format$pandoc$args, args)
    output_format
  }

  # post processor that renders our markdown using out custom lua
  # renderer and then inserts it into the main file
  post_processor <- function(input_file, output_file, verbose) {

    # smart, mathjax, etc.
    args <- c()
    if (smart)
      args <- c(args, "--smart")
    if (!is.null(mathjax))
      args <- c(args, "--mathjax")

    # convert using our lua writer (write output to a temp file)
    lua_writer <- tempfile("ioslides", fileext = ".lua")

    # write settings to file
    settings <- c()
    add_setting <- function(name, value) {
      settings <<- c(settings, paste("local", name, "=",
                                    ifelse(value, "true", "false")))
    }
    add_setting("fig_caption", fig_caption)
    add_setting("incremental", incremental)
    add_setting("base64_images", self_contained)
    add_setting("smaller", smaller)
    writeLines(settings, lua_writer, useBytes = TRUE)

    # append main body of script
    file.append(lua_writer,
                rmarkdown_system_file("rmd/ioslides/slides.lua"))

    output_tmpfile <- tempfile("ioslides-output", fileext = ".html")
    on.exit(unlink(output_tmpfile), add = TRUE)
    pandoc_convert(input = input_file,
                   to = pandoc_path_arg(lua_writer),
                   from = from_rmarkdown(fig_caption),
                   output = output_tmpfile,
                   options = args,
                   verbose = verbose)
    slides_lines <- readLines(output_tmpfile, warn = FALSE, encoding = "UTF-8")

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
                            from_rmarkdown(fig_caption),
                            args = args),
    clean_supporting = self_contained,
    format_filter = format_filter,
    post_processor = post_processor
  )
}

