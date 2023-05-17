#' Convert to an ioslides Presentation
#'
#' Format for converting from R Markdown to an
#' [ioslides](https://code.google.com/archive/p/io-2012-slides/) presentation.
#'
#' @md
#' @inheritParams html_document
#' @param logo Path to file that includes a logo for use in the presentation
#'   (should be square and at least 128x128).
#' @param slide_level Header level to consider as slide separator (Defaults to
#'   header 2).
#' @param incremental `TRUE` to render slide bullets incrementally.
#'   Note that if you want to reverse the default incremental behavior for an
#'   individual bullet you can preceded it with `>`.
#'   For example: *`> - Bullet Text`*.
#' @param widescreen Display presentation with wider dimensions.
#' @param smaller Use smaller text on all slides. You can also enable this for
#'   individual slides by adding the `.smaller` attribute to the slide
#'   header (see *Presentation Size* below for details).
#' @param transition Speed of slide transitions. This can be "default",
#'   "slower", "faster", or a numeric value with a number of seconds (e.g. 0.5).
#' @param analytics A Google analytics property ID.
#' @param smart Produce typographically correct output, converting straight
#'  quotes to curly quotes, `---` to em-dashes, `--` to en-dashes, and
#'  `...` to ellipses.
#' @param css One or more css files to include.
#' @return R Markdown output format to pass to [render()].
#' @details
#'   See the [
#'   online documentation](https://bookdown.org/yihui/rmarkdown/ioslides-presentation.html) for additional details on using the
#'   `ioslides_presentation` format.
#'
#'   Note that, if a `before_body` include is specified in `includes`,
#'   then it will replace the standard title slide entirely.
#'
#'   Regarding previewing slide in RStudio IDE, `ioslides_presentation()` will
#'   always open preview in a new Window and the RStudio IDE configuration "Open in Viewer
#'   Pane" will have no effect for this format.
#'
#' @section Slide Basics:
#'   You can create a slide show broken up into sections by using the # and ##
#'   heading tags (you can also create a new slide without a header using a
#'   horizontal rule (`----------`). For example here's a simple slide show:
#'   ````markdown
#'   ---
#'   title: "Habits"
#'   author: John Doe
#'   date: March 22, 2005
#'   output: ioslides_presentation
#'   ---
#'
#'   # In the morning
#'
#'   ## Getting up
#'
#'   - Turn off alarm
#'   - Get out of bed
#'
#'   ## Breakfast
#'
#'   - Eat eggs
#'   - Drink coffee
#'
#'   # In the evening
#'
#'   ## Dinner
#'
#'   - Eat spaghetti
#'   - Drink wine
#'
#'   ----------
#'
#'   ![picture of spaghetti](images/spaghetti.jpg)
#'
#'   ## Going to sleep
#'
#'   - Get in bed
#'   - Count sheep
#'  ````
#'
#'   You can add a subtitle to a slide or section by including text after the
#'   pipe (|) character. For example:
#'   ```markdown
#'   ## Getting up | What I like to do first thing
#'   ```
#'
#' @section Display Modes:
#'   The following single character keyboard shortcuts enable alternate display
#'   modes:
#'   \itemize{
#'     \item{`'f'`      }{enable fullscreen mode}
#'     \item{`'w'`      }{toggle widescreen mode}
#'     \item{`'o'`      }{enable overview mode}
#'     \item{`'h'`      }{enable code highlight mode}
#'     \item{`'p'`      }{show presenter notes}
#'   }
#'   Pressing `Esc` exits all of these modes. See the sections below on
#'   *Code Highlighting* and *Presenter Mode* for additional
#'   detail on those modes.
#'
#' @section Incremental Bullets:
#'   You can render bullets incrementally by adding the `incremental`
#'   option:
#'   ```markdown
#'   ---
#'   output:
#'     ioslides_presentation:
#'       incremental: true
#'   ---
#'   ```
#'   If you want to render bullets incrementally for some slides but not
#'   others you can use this syntax:
#'   ```
#'   > - Eat eggs
#'   > - Drink coffee
#'   ```
#'
#' @section Presentation Size:
#'   You can display the presentation using a wider form factor using the
#'   `widescreen` option. You can specify that smaller text be used with
#'   the `smaller` option. For example:
#'   ````yaml
#'   ---
#'   output:
#'     ioslides_presentation:
#'       widescreen: true
#'       smaller: true
#'   ---
#'   ````
#'   You can also enable the `smaller` option on a slide-by-slide basis
#'   by adding the `.smaller` attribute to the slide header:
#'   ```markdown
#'   ## Getting up {.smaller}
#'   ```
#' @section Adding a Logo:
#'   You can add a logo to the presentation using the `logo` option (the
#'   logo should be square and at least 128x128). For example:
#'   ```markdown
#'   ---
#'   output:
#'     ioslides_presentation:
#'       logo: logo.png
#'   ---
#'   ```
#'   A 128x128 version of the logo graphic will be added to the title slide and
#'   an icon version of the logo will be included in the bottom-left footer of
#'   each slide.
#' @section Build Slides:
#'   Slides can also have a `.build` attribute that indicate that their
#'   content should be displayed incrementally. For example:
#'   ```markdown
#'   ## Getting up {.build}
#'   ```
#'   Slide attributes can be combined if you need to specify more than one,
#'   for example:
#'   ```markdown
#'   ## Getting up {.smaller .build}
#'   ```
#' @section Code Highlighting:
#'   It's possible to select subsets of code for additional emphasis by adding a
#'   special "highlight" comment around the code. For example:
#'   ```markdown
#'   ### <b>
#'   x <- 10
#'   y <- x * 2
#'   ### </b>
#'   ```
#'   The highlighted region will be displayed with a bold font. When you want to
#'   help the audience focus exclusively on the highlighted region press the
#'   `'h'` key and the rest of the code will fade away.
#' @section Tables:
#'   The ioslides template has an attractive default style for tables so you
#'   shouldn't hesitate to add tables for presenting more complex sets of
#'   information. Pandoc markdown supports several syntaxes for defining
#'   tables which are described in the
#'   [pandoc online documentation](https://pandoc.org/MANUAL.html).
#' @section Advanced Layout:
#'   You can center content on a slide by adding the `.flexbox`
#'   and `.vcenter` attributes to the slide title. For example:
#'   ```markdown
#'   ## Dinner {.flexbox .vcenter}
#'   ```
#'   You can horizontally center content by enclosing it in a `div` tag
#'   with class `centered`. For example:
#'   ```html
#'   <div class="centered">
#'   This text is centered.
#'   </div>
#'   ```
#'   You can do a two-column layout using the `columns-2` class.
#'   For example:
#'   ```
#'   <div class="columns-2">
#'     ![Image](image.png)
#'
#'     - Bullet 1
#'     - Bullet 2
#'     - Bullet 3
#'   </div>
#'   ```
#'   Note that content will flow across the columns so if you want to
#'   have an image on one side and text on the other you should make
#'   sure that the image has sufficient height to force the text to
#'   the other side of the slide.
#' @section Text Color:
#'   You can color content using base color classes red, blue, green, yellow,
#'   and gray (or variations of them e.g. red2, red3, blue2, blue3, etc.).
#'   For example:
#'   ```html
#'   <div class="red2">
#'   This text is red
#'   </div>
#'   ```
#' @section Presenter Mode:
#'   A separate presenter window can also be opened (ideal for when you are
#'   presenting on one screen but have another screen that's private to you).
#'   The window stays in sync with the main presentation window and also
#'   shows presenter notes and a thumbnail of the next slide. To enable
#'   presenter mode add `?presentme=true` to the URL of the presentation,
#'   for example:
#'   ```
#'   mypresentation.html?presentme=true
#'   ```
#'   The presenter mode window will open and will always re-open with the
#'   presentation until it's disabled with:
#'   ```
#'   mypresentation.html?presentme=false
#'   ```
#'   To add presenter notes to a slide you include it within a "notes"
#'   `div`. For example:
#'   ```
#'   <div class="notes">
#'   This is my *note*.
#'
#'   - It can contain markdown
#'   - like this list
#'
#'   </div>
#'   ```
#' @section Printing and PDF Output:
#'   You can print an ioslides presentation from within browsers that have
#'   good support for print CSS (i.e. as of this writing Google Chrome
#'   has the best support). Printing maintains most of the visual styles
#'   of the HTML version of the presentation.
#'
#'   To create a PDF version of a presentation you can use Print to PDF
#'   from Google Chrome.
#' @export
ioslides_presentation <- function(number_sections = FALSE,
                                  logo = NULL,
                                  slide_level = 2,
                                  incremental = FALSE,
                                  fig_width = 7.5,
                                  fig_height = 4.5,
                                  fig_retina = 2,
                                  fig_caption = TRUE,
                                  dev = 'png',
                                  df_print = "default",
                                  smart = TRUE,
                                  self_contained = TRUE,
                                  widescreen = FALSE,
                                  smaller = FALSE,
                                  transition = "default",
                                  math_method = "mathjax",
                                  mathjax = "default",
                                  analytics = NULL,
                                  template = NULL,
                                  css = NULL,
                                  includes = NULL,
                                  keep_md = FALSE,
                                  lib_dir = NULL,
                                  md_extensions = NULL,
                                  pandoc_args = NULL,
                                  extra_dependencies = NULL,
                                  ...) {

  # base pandoc options for all output
  args <- c()

  # math
  math <- mathjax_to_math(mathjax, math_method)
  math <- check_math_argument(math)
  if (!identical(math$engine, "mathjax")) {
    stop2("Only mathjax is supported for `ioslide_presentation()` for 'math'.")
  }

  # widescreen
  if (widescreen)
    args <- c(args, "--variable", "widescreen");

  # pagedtables
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies,
                                 list(html_dependency_pagedtable()))

  }

  # transition
  if (is.numeric(transition))
    transition <- as.character(transition)
  else if (transition %in% c("default", "faster", "slower"))
    transition <- switch(transition,
                         "default" = "0.4",
                         "faster" = "0.2",
                         "slower" = "0.6")
  else
    stop2('transition must be "default", "faster", "slower" or a ',
         'numeric value (representing seconds)')
  args <- c(args, pandoc_variable_arg("transition", transition))

  # additional css
  for (css_file in css)
    args <- c(args, "--css", pandoc_path_arg(css_file, backslash = FALSE))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # template path and assets
  if (is.null(template) || !file.exists(template))
    template <- pkg_file("rmd/ioslides/default.html")
  args <- c(args, "--template", pandoc_path_arg(template))

  # html dependency for ioslides
  extra_dependencies <- append(extra_dependencies,
                               list(html_dependency_ioslides()))

  # analytics
  if (!is.null(analytics))
    args <- c(args, pandoc_variable_arg("analytics", analytics))

  # do not wrap lines: https://github.com/rstudio/rmarkdown/issues/2327
  if (!length(grep('--wrap', pandoc_args)))
    pandoc_args <- c('--wrap', 'none', pandoc_args)

  logo_placeholder <- "data:,LOGO"

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
        logo_ext <- xfun::file_ext(logo)
        if (nchar(logo_ext) < 1)
          logo_ext <- "png"
        logo_path <- file.path(files_dir, paste("logo", logo_ext, sep = "."))
        file.copy(from = logo, to = logo_path)
        logo_path <- normalized_relative_to(output_dir, logo_path)
      } else {
        # placeholder, will be replaced by base64-encoded logo in post_processor
        logo_path <- logo_placeholder
      }
      args <- c(args, "--variable", paste("logo=", logo_path, sep = ""))
    }

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

    # number sections
    if (number_sections)
      args <- c(args, pandoc_lua_filter_args(pkg_file_lua("number-sections.lua")))

    lua_writer <- file.path(dirname(input_file), "ioslides_presentation.lua")
    # The input directory may not be writable (on e.g. Shiny Server), so write
    # to the output directory in this case. We don't always do this since
    # supplying a fully qualified path to the writer can trigger a bug on some
    # Linux configurations.
    if (!file.create(lua_writer, showWarnings = FALSE))
      lua_writer <- file.path(dirname(output_file), basename(lua_writer))
    on.exit(unlink(lua_writer), add = TRUE)

    # determine whether we need to run citeproc
    run_citeproc <- citeproc_required(metadata, read_utf8(input_file))

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

    # Set level of slide header (used by ioslides_presentation.lua)
    settings <- c(settings, sprintf("local slide_level = %s", slide_level))
    write_utf8(settings, lua_writer)

    # For consistency add as pandoc argument
    args <- c(args, "--slide-level", as.character(slide_level))

    # append main body of script
    file.append(lua_writer,
                pkg_file("rmd/ioslides/ioslides_presentation.lua"))

    output_tmpfile <- tempfile("ioslides-output", fileext = ".html")
    on.exit(unlink(output_tmpfile), add = TRUE)

    # on Windows, cache the current codepage and set it to 65001 (UTF-8) for the
    # duration of the Pandoc command. Without this, Pandoc fails when attempting
    # to hand UTF-8 encoded non-ASCII characters over to the custom Lua writer.
    # See https://github.com/rstudio/rmarkdown/issues/134
    if (is_windows() && !pandoc2.0()) {
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
    slides_lines <- read_utf8(output_tmpfile)

    # read the output file
    output_lines <- read_utf8(output_file)

    # base64 encode if needed
    if (self_contained) {
      slides_lines <- base64_encode_images(slides_lines)
      if (!is.null(logo)) {
        logo_base64 <- if (grepl("^data:", logo)) logo else xfun::base64_uri(logo)
        output_lines <- gsub(logo_placeholder, logo_base64, output_lines, fixed = TRUE)
      }
    }

    # substitute slides for the sentinel line
    sentinel_line <- grep("^RENDERED_SLIDES$", output_lines)
    if (length(sentinel_line) == 1) {
      preface_lines <- c(output_lines[1:sentinel_line[1] - 1])
      suffix_lines <- c(output_lines[-(1:sentinel_line[1])])
      output_lines <- c(preface_lines, slides_lines, suffix_lines)
      write_utf8(output_lines, output_file)
    } else {
      stop2("Slides placeholder not found in slides HTML")
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
    df_print = df_print,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = html_document_base(lib_dir = lib_dir,
                                     self_contained = self_contained,
                                     mathjax = mathjax,
                                     pandoc_args = pandoc_args,
                                     extra_dependencies = extra_dependencies,
                                     bootstrap_compatible = TRUE, ...))
}


html_dependency_ioslides <- function() {
  htmlDependency(
    name = "ioslides",
    version = "13.5.1",
    src = pkg_file("rmd/ioslides/ioslides-13.5.1"),
    script = c(
      "js/modernizr.custom.45394.js",
      "js/prettify/prettify.js",
      "js/prettify/lang-r.js",
      "js/prettify/lang-yaml.js",
      "js/hammer.js",
      "js/slide-controller.js",
      "js/slide-deck.js"
    ),
    stylesheet = c(
      "fonts/fonts.css",
      "theme/css/default.css",
      "theme/css/phone.css")
    )
}
