

ioslides_presentation <- function(fig_width = 8,
                                  fig_height = 6,
                                  fig_retina = 2,
                                  fig_caption = FALSE,
                                  smart = TRUE,
                                  self_contained = TRUE,
                                  mathjax = "default",
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

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina),
    pandoc = pandoc_options(to = "html",
                            from_rmarkdown(fig_caption),
                            args = args),
    clean_supporting = self_contained,
    format_filter = format_filter,
    input_filter = lua_input_filter(
                        rmarkdown_system_file("rmd/ioslides/slides.lua"))
  )
}

lua_input_filter <- function(lua_script) {

  function(input_text) {

    # write to a temp file for pandoc
    input_tmpfile <- tempfile("lua-input", fileext = ".md")
    writeLines(text = input_text,
               con = input_tmpfile,
               useBytes = TRUE)
    on.exit(unlink(input_tmpfile), add = TRUE)

    # write to another tempfile
    output_tmpfile <- tempfile("lua-output", fileext = ".md")
    on.exit(unlink(output_tmpfile), add = TRUE)

    # do the convert
    pandoc_convert(input = input_tmpfile,
                   to = lua_script,
                   output = output_tmpfile)

    # return the output
    readLines(output_tmpfile, warn = FALSE)
  }
}
