#' Convert to an HTML document
#'
#' Format for converting from R Markdown to an HTML document.
#'
#' See the \href{https://bookdown.org/yihui/rmarkdown/html-document.html}{online
#' documentation} for additional details on using the \code{html_document}
#' format.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations in the
#' \href{https://pandoc.org/MANUAL.html#citations}{Bibliographies
#' and Citations} article in the online documentation.
#'
#'@inheritParams output_format
#'@param toc \code{TRUE} to include a table of contents in the output
#'@param toc_depth Depth of headers to include in table of contents
#'@param toc_float \code{TRUE} to float the table of contents to the left of the
#'  main document content. Rather than \code{TRUE} you may also pass a list of
#'  options that control the behavior of the floating table of contents. See the
#'  \emph{Floating Table of Contents} section below for details.
#'@param number_sections \code{TRUE} to number section headings
#'@param anchor_sections \code{TRUE} to show section anchors when mouse hovers.
#'  See \link[rmarkdown:html_document]{Anchor Sections Customization section}.
#'@param fig_width Default width (in inches) for figures
#'@param fig_height Default height (in inches) for figures
#'@param fig_retina Scaling to perform for retina displays (defaults to 2, which
#'  currently works for all widely used retina displays). Set to \code{NULL} to
#'  prevent retina scaling. Note that this will always be \code{NULL} when
#'  \code{keep_md} is specified (this is because \code{fig_retina} relies on
#'  outputting HTML directly into the markdown document).
#'@param fig_caption \code{TRUE} to render figures with captions
#'@param dev Graphics device to use for figure output (defaults to png)
#'@param code_folding Enable document readers to toggle the display of R code
#'  chunks. Specify \code{"none"} to display all code chunks (assuming
#'  they were knit with \code{echo = TRUE}). Specify \code{"hide"} to hide all R
#'  code chunks by default (users can show hidden code chunks either
#'  individually or document-wide). Specify \code{"show"} to show all R code
#'  chunks by default.
#'@param code_download Embed the Rmd source code within the document and provide
#'  a link that can be used by readers to download the code.
#'@param self_contained Produce a standalone HTML file with no external
#'  dependencies, using data: URIs to incorporate the contents of linked
#'  scripts, stylesheets, images, and videos. Note that even for self contained
#'  documents MathJax is still loaded externally (this is necessary because of
#'  its size).
#'@param theme One of the following:
#'  * A [bslib::bs_theme()] object (or a list of [bslib::bs_theme()] argument values)
#'    * Use this option for custom themes using Bootstrap 4 or 3.
#'    * In this case, any `.scss`/`.sass` files provided to the `css`
#'      parameter may utilize the `theme`'s underlying Sass utilities
#'      (e.g., variables, mixins, etc).
#'  * `NULL` for no theme (i.e., no [html_dependency_bootstrap()]).
#'  * A character string specifying a [Bootswatch 3](https://bootswatch.com/3/)
#'    theme name (for backwards-compatibility).
#'@param highlight Syntax highlighting style. Supported styles include
#'  "default", "tango", "pygments", "kate", "monochrome", "espresso", "zenburn",
#'  "haddock", and "textmate". Pass \code{NULL} to prevent syntax highlighting.
#'@param mathjax Include mathjax. The "default" option uses an https URL from a
#'  MathJax CDN. The "local" option uses a local version of MathJax (which is
#'  copied into the output directory). You can pass an alternate URL or pass
#'  \code{NULL} to exclude MathJax entirely.
#'@param section_divs Wrap sections in \code{<div>} tags, and attach identifiers to the
#'  enclosing \code{<div>} rather than the header itself.
#'@param template Pandoc template to use for rendering. Pass "default" to use
#'  the rmarkdown package default template; pass \code{NULL} to use pandoc's
#'  built-in template; pass a path to use a custom template that you've created.
#'  Note that if you don't use the "default" template then some features of
#'  \code{html_document} won't be available (see the Templates section below for
#'  more details).
#'@param css CSS and/or Sass files to include. Files with an extension of .sass
#'  or .scss are compiled to CSS via `sass::sass()`. Also, if `theme` is a
#'  [bslib::bs_theme()] object, Sass code may reference the relevant Bootstrap
#'  Sass variables, functions, mixins, etc.
#'@param includes Named list of additional content to include within the
#'  document (typically created using the \code{\link{includes}} function).
#'@param keep_md Keep the markdown file generated by knitting.
#'@param lib_dir Directory to copy dependent HTML libraries (e.g. jquery,
#'  bootstrap, etc.) into. By default this will be the name of the document with
#'  \code{_files} appended to it.
#'@param md_extensions Markdown extensions to be added or removed from the
#'  default definition or R Markdown. See the \code{\link{rmarkdown_format}} for
#'  additional details.
#'@param pandoc_args Additional command line options to pass to pandoc
#'@param extra_dependencies,allow_uptree_lib_dir,... Additional function
#'  arguments to pass to the
#'  base R Markdown HTML output formatter \code{\link{html_document_base}}
#'@return R Markdown output format to pass to \code{\link{render}}
#'
#'@section Anchor Sections Customization:
#'  By default, a \samp{#} is used as a minimalist choice, referring to the id selector
#'  in HTML and CSS. You can easily change that using a css rule in your
#'  document. For example, to add a \href{https://codepoints.net/U+1F517}{link
#'  symbol} \if{html}{\out{(&#x1F517;&#xFE0E;)}} instead:
#'  \preformatted{
#'  a.anchor-section::before {
#'    content: '\\01F517\\00FE0E';
#'  }}
#'  You can remove \samp{\\00FE0E} to get a more complex link pictogram
#'  \if{html}{\out{(&#x1F517;)}}.
#'
#'  If you prefer an svg icon, you can also use one using for example a direct link or downloading it from
#'  \url{https://fonts.google.com/icons}.
#'  \preformatted{
#'  /* From https://fonts.google.com/icons
#'     Licence: https://www.apache.org/licenses/LICENSE-2.0.html */
#'  a.anchor-section::before {
#'    content: url(https://fonts.gstatic.com/s/i/materialicons/link/v7/24px.svg);
#'  }}
#'
#'  About how to apply custom CSS, see
#'  \url{https://bookdown.org/yihui/rmarkdown-cookbook/html-css.html}
#'
#'@section Navigation Bars:
#'
#'  If you have a set of html documents which you'd like to provide a common
#'  global navigation bar for, you can include a "_navbar.yml" or "_navbar.html"
#'  file within the same directory as your html document and it will automatically
#'  be included at the top of the document.
#'
#'  The "_navbar.yml" file includes \code{title}, \code{type}, \code{left}, and
#'  \code{right} fields (to define menu items for the left and right of the navbar
#'  respectively). Menu items include \code{title} and \code{href} fields. For example:
#'
#'  \preformatted{title: "My Website"
#' type: default
#' left:
#'   - text: "Home"
#'     href: index.html
#'   - text: "Other"
#'     href: other.html
#' right:
#'   - text: GitHub
#'     href: https://github.com}
#'  The \code{type} field is optional and can take the value "default" or "inverse" (which
#'  provides a different color scheme for the navigation bar).
#'
#'  Alternatively, you can include a "_navbar.html" file which is a full HTML definition
#'  of a bootstrap navigation bar. For a simple example of including a navigation bar see
#'  \url{https://github.com/rstudio/rmarkdown-website/blob/master/_navbar.html}.
#'   For additional documentation on creating Bootstrap navigation bars see
#'  \url{https://getbootstrap.com/docs/4.5/components/navbar/}.
#'
#'
#'@section Floating Table of Contents:
#'
#'  You may specify a list of options for the \code{toc_float} parameter which
#'  control the behavior of the floating table of contents. Options include:
#'
#'  \itemize{ \item{\code{collapsed} (defaults to \code{TRUE}) controls whether
#'  the table of contents appears with only the top-level (H2) headers. When
#'  collapsed the table of contents is automatically expanded inline when
#'  necessary.} \item{\code{smooth_scroll} (defaults to \code{TRUE}) controls
#'  whether page scrolls are animated when table of contents items are navigated
#'  to via mouse clicks.} \item{\code{print} (defaults to \code{TRUE}) controls
#'  whether the table of contents appears when user prints out the HTML page.}}
#'
#'@section Tabbed Sections:
#'
#'  You can organize content using tabs by applying the \code{.tabset} class
#'  attribute to headers within a document. This will cause all sub-headers of
#'  the header with the \code{.tabset} attribute to appear within tabs rather
#'  than as standalone sections. For example:
#'
#'  \preformatted{## Quarterly Results {.tabset}
#'
#' ### By Product
#'
#' ### By Region }
#'
#'  You can also specify two additional attributes to control the appearance and
#'  behavior of the tabs. The \code{.tabset-fade} attributes causes the tabs to
#'  fade in and out when switching. The \code{.tabset-pills} attribute causes
#'  the visual appearance of the tabs to be "pill" rather than traditional tabs.
#'  For example:
#'
#'  \preformatted{## Quarterly Results {.tabset .tabset-fade .tabset-pills}}
#'
#'@section Templates:
#'
#'  You can provide a custom HTML template to be used for rendering. The syntax
#'  for templates is described in the
#'  \href{https://pandoc.org/MANUAL.html}{pandoc documentation}. You can also use
#'  the basic pandoc template by passing \code{template = NULL}.
#'
#'  Note however that if you choose not to use the "default" HTML template then
#'  several aspects of HTML document rendering will behave differently:
#'
#'  \itemize{ \item{The \code{theme} parameter does not work (you can still
#'  provide styles using the \code{css} parameter). } \item{For the
#'  \code{highlight} parameter, the default highlighting style will resolve to
#'  "pygments" and the "textmate" highlighting style is not available }
#'  \item{The \code{toc_float} parameter will not work. } \item{The
#'  \code{code_folding} parameter will not work. } \item{Tabbed sections (as
#'  described above) will not work.} \item{Navigation bars (as described above)
#'  will not work. }\item{MathJax will not work if \code{self_contained} is
#'  \code{TRUE} (these two options can't be used together in normal pandoc
#'  templates). } }
#'
#'  Due to the above restrictions, you might consider using the \code{includes}
#'  parameter as an alternative to providing a fully custom template.
#'
#'@section Directory structure:
#'
#'  By default \code{html_document} and related HTML document types put
#'  dependency files into the main output directory or a subdirectory specified
#'  by the \code{lib_dir} parameter:
#'  If \code{lib_dir} is not a direct descendant of the main output directory,
#'  \code{render()} will throw and error with the message
#'  "The path &lt;file&gt; does not appear to be a descendant of &lt;dir&gt;".
#'
#'  Sometimes it is useful to have a directory tree where the different
#'  HTML documents are in their own subdirectories and the dependencies are in
#'  a common directory at the root of the site.
#'
#'  \itemize{
#'  \item{
#'  \code{main_dir/}
#'  \itemize{
#'  \item{
#'  \code{lib/}
#'  \itemize{
#'  \item{dependencies go here}
#'  }
#'  }
#'  }
#'  \item{\code{index.Rmd}}
#'  \item{
#'  \code{node-01/}
#'  \itemize{
#'  \item{\code{index.Rmd}}
#'  }
#'  }
#'  \item{
#'  \code{node-02/}
#'  \itemize{
#'  \item{\code{index.Rmd}}
#'  }
#'  }
#'  }
#'  }
#'
#'  One way to achieve this is with the `render_site` command, but knitting
#'  individual documents in subdirectories (such as with the RStudio "knit"
#'  button) will result in errors.
#'
#'  It is possible to achieve this directory structure by setting the
#'  `allow_uptree_lib_dir` parameter to `yes` or `true` in the
#'  `output/html_document` section of the YAML header
#'  and set `lib_dir` to a relative path, such as `../lib` or `../site_lib`
#'  in `node-01/index.Rmd` and `node-02/index.Rmd` in the example above.
#'
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' render("input.Rmd", html_document())
#'
#' render("input.Rmd", html_document(toc = TRUE))
#' }
#' @md
#' @export
html_document <- function(toc = FALSE,
                          toc_depth = 3,
                          toc_float = FALSE,
                          number_sections = FALSE,
                          anchor_sections = FALSE,
                          section_divs = TRUE,
                          fig_width = 7,
                          fig_height = 5,
                          fig_retina = 2,
                          fig_caption = TRUE,
                          dev = 'png',
                          df_print = "default",
                          code_folding = c("none", "show", "hide"),
                          code_download = FALSE,
                          self_contained = TRUE,
                          theme = "default",
                          highlight = "default",
                          mathjax = "default",
                          template = "default",
                          extra_dependencies = NULL,
                          css = NULL,
                          includes = NULL,
                          keep_md = FALSE,
                          lib_dir = NULL,
                          md_extensions = NULL,
                          pandoc_args = NULL,
                          allow_uptree_lib_dir = FALSE,
                          ...) {

  # build pandoc args
  args <- c("--standalone")

  # use section divs
  if (section_divs)
    args <- c(args, "--section-divs")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # makes downstream logic easier to reason about
  theme <- resolve_theme(theme)

  # toc_float
  if (toc && !identical(toc_float, FALSE)) {

    # must have a theme
    if (is.null(theme))
      stop("You must use a theme when specifying the 'toc_float' option")

    # resolve options
    toc_float_options <- list(collapsed = TRUE,
                              smooth_scroll = TRUE,
                              print = TRUE)
    if (is.list(toc_float)) {
      toc_float_options <- merge_lists(toc_float_options, toc_float)
      toc_float <- TRUE
    } else if (!isTRUE(toc_float)) {
      stop("toc_float must be a logical or a list with options")
    }

    # dependencies
    extra_dependencies <- append(extra_dependencies,
                                 list(html_dependency_jquery(),
                                      html_dependency_jqueryui(),
                                      html_dependency_tocify()))

    # flag for template
    args <- c(args, pandoc_variable_arg("toc_float", "1"))

    # selectors
    selectors <- paste0("h", seq(1, toc_depth), collapse = ",")
    args <- c(args, pandoc_variable_arg("toc_selectors", selectors))

    # options
    if (toc_float_options$collapsed)
      args <- c(args, pandoc_variable_arg("toc_collapsed", "1"))
    if (toc_float_options$smooth_scroll)
      args <- c(args, pandoc_variable_arg("toc_smooth_scroll", "1"))
    if (toc_float_options$print)
      args <- c(args, pandoc_variable_arg("toc_print", "1"))
  }

  # template path and assets
  template_file <- if (identical(template, "default")) {
    pkg_file("rmd/h/default.html")
  } else template
  if (!is.null(template_file))
    args <- c(args, "--template", pandoc_path_arg(template_file))

  # validate code_folding
  code_folding <- match.arg(code_folding)

  # navigation dependencies
  if (!is.null(theme)) {
    code_menu <- !identical(code_folding, "none") || code_download
    source_embed <- code_download
    extra_dependencies <- append(extra_dependencies,
      list(
        html_dependency_jquery(),
        html_dependency_navigation(code_menu = code_menu,
                                   source_embed = source_embed)
      )
    )
  }

  # highlight
  args <- c(args, pandoc_html_highlight_args(template, highlight))

  # add highlight.js html_dependency if required
  extra_dependencies <- append(
    extra_dependencies,
    if (identical(template, "default") && is_highlightjs(highlight)) {
      list(html_dependency_highlightjs(highlight))
    } else if (!is.null(highlight)) {
      # for screen-reader accessibility improvement
      list(html_dependency_accessible_code_block())
    }
  )

  # numbered sections
  if (number_sections)
    args <- c(args, "--number-sections")


  # manage list of exit_actions (backing out changes to knitr options)
  exit_actions <- list()
  on_exit <- function() {
    for (action in exit_actions)
      try(action())
  }

  # capture the source code if requested
  source_code <- NULL
  source_file <- NULL
  pre_knit <- function(input, ...) {
    if (code_download) {
      source_file <<- basename(input)
      source_code <<- paste0(
        '<div id="rmd-source-code">',
        xfun::base64_encode(input),
        '</div>')
    }
  }

  # pagedtable
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies,
                                 list(html_dependency_pagedtable()))
  }

  # anchor-sections
  if (anchor_sections) {
    extra_dependencies <- append(extra_dependencies,
                                 list(html_dependency_anchor_sections()))
  }

  # pre-processor for arguments that may depend on the name of the
  # the input file AND which need to inject html dependencies
  # (otherwise we could just call the pre_processor)
  post_knit <- function(metadata, input_file, runtime, ...) {

    # extra args
    args <- c()

    # navbar (requires theme)
    if (!is.null(theme)) {

      # add navbar to includes if necessary
      navbar <- file.path(normalize_path(dirname(input_file)), "_navbar.html")

      # if there is no _navbar.html look for a _navbar.yml
      if (!file.exists(navbar)) {
        navbar_yaml <- file.path(dirname(navbar), "_navbar.yml")
        if (file.exists(navbar_yaml))
          navbar <- navbar_html_from_yaml(navbar_yaml)
        # if there is no _navbar.yml then look in site config (if we have it)
        config <- site_config(input_file)
        if (!is.null(config) && !is.null(config$navbar))
          navbar <- navbar_html(config$navbar)
      }

      if (file.exists(navbar)) {

        # include the navbar html
        includes <- list(before_body = navbar)
        args <- c(args, includes_to_pandoc_args(includes,
                                  filter = if (is_shiny_classic(runtime))
                                    function(x) normalize_path(x, mustWork = FALSE)
                                  else
                                    identity))

        # flag indicating we need extra navbar css and js
        args <- c(args, pandoc_variable_arg("navbar", "1"))

        # navbar icon dependencies
        iconDeps <- navbar_icon_dependencies(navbar)
        if (length(iconDeps) > 0)
          knitr::knit_meta_add(list(iconDeps))
      }
    }

    args
  }

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # extra args
    args <- c()

    # track whether we have a code menu
    code_menu <- FALSE

    # code_folding
    if (code_folding %in% c("show", "hide")) {
      # must have a theme
      if (is.null(theme))
        stop("You must use a theme when specifying the 'code_folding' option")
      args <- c(args, pandoc_variable_arg("code_folding", code_folding))
      code_menu <- TRUE
    }

    # source_embed
    if (code_download) {
      if (is.null(theme))
        stop("You must use a theme when specifying the 'code_download' option")
      args <- c(args, pandoc_variable_arg("source_embed", source_file))
      sourceCodeFile <- tempfile(fileext = ".html")
      write_utf8(source_code, sourceCodeFile)
      args <- c(args, pandoc_include_args(after_body = sourceCodeFile))
      code_menu <- TRUE
    }

    # code menu
    if (code_menu)
      args <- c(args, pandoc_variable_arg("code_menu", "1"))

    # content includes (we do this here so that user include-in-header content
    # goes after dependency generated content). make the paths absolute if
    # making a Shiny document so we can resolve them even if rendering
    # elsewhere.
    args <- c(args, includes_to_pandoc_args(includes,
                      filter = if (is_shiny_classic(runtime))
                        function(x) normalize_path(x, mustWork = FALSE)
                      else
                        identity))

    # return additional args
    args
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
    pre_knit = pre_knit,
    post_knit = post_knit,
    pre_processor = pre_processor,
    on_exit = on_exit,
    base_format = html_document_base(theme = theme,
                                     self_contained = self_contained,
                                     lib_dir = lib_dir, mathjax = mathjax,
                                     template = template,
                                     pandoc_args = pandoc_args,
                                     extra_dependencies = extra_dependencies,
                                     css = css,
                                     allow_uptree_lib_dir = allow_uptree_lib_dir,
                                     ...)
  )
}


#' Knitr options for an HTML output format
#'
#' Define knitr options for an R Markdown output format that creates
#' HTML output.
#'
#' @inheritParams html_document
#' @return An list that can be passed as the \code{knitr} argument of the
#'   \code{\link{output_format}} function.
#' @seealso \link{knitr_options}, \link{output_format}
#' @export
knitr_options_html <- function(fig_width,
                               fig_height,
                               fig_retina,
                               keep_md,
                               dev = 'png') {

  opts_chunk <- list(dev = dev,
                     dpi = 96,
                     fig.width = fig_width,
                     fig.height = fig_height,
                     fig.retina = fig_retina)

  if (keep_md)
    opts_chunk$fig.retina <- NULL

  knitr_options(opts_chunk = opts_chunk)
}

# CSS files in inst/rmd/h/bootstrap/css
themes <- function() {
  c("default", # keep for backward compatibility reason, changed to 'bootstrap' internally
    "bootstrap",
    "cerulean",
    "cosmo",
    "darkly",
    "flatly",
    "journal",
    "lumen",
    "paper",
    "readable",
    "sandstone",
    "simplex",
    "spacelab",
    "united",
    "yeti")
}

html_highlighters <- function() {
  c(highlighters(), "textmate")
}

default_mathjax <- function() {
  paste0("https://mathjax.rstudio.com/latest/", mathjax_config())
}

mathjax_config <- function() {
  "MathJax.js?config=TeX-AMS-MML_HTMLorMML"
}


navbar_html_from_yaml <- function(navbar_yaml) {

  # parse the yaml
  navbar <- yaml_load_file(navbar_yaml)

  # generate the html
  navbar_html(navbar)
}


#' Create a navbar HTML file from a navbar definition
#'
#' @param navbar Navbar definition
#' @param links List of navbar links
#' @return Path to temporary file with navbar definition
#' @keywords internal
#' @export
navbar_html <- function(navbar) {

  # title and type
  if (is.null(navbar$title)) navbar$title <- ""
  if (is.null(navbar$type)) navbar$type <- "default"

  # menu entries
  left <- navbar_links_html(navbar$left)
  right <- navbar_links_html(navbar$right)

  # build the navigation bar and return it as a temp file
  template <- file_string(pkg_file("rmd/h/_navbar.html"))
  navbar_html <- sprintf(template, navbar$type, navbar$title, left, right)
  as_tmpfile(navbar_html)
}

#' @keywords internal
#' @name navbar_html
#' @export
navbar_links_html <- function(links) {
  as.character(navbar_links_tags(links))
}

navbar_links_tags <- function(links, depth = 0L) {

  if (!is.null(links)) {

    tags <- lapply(links, function(x) {

      if (!is.null(x$menu)) {

        # sub-menu
        is_submenu <- depth > 0L

        if (is_submenu) {
          menu_class <- "dropdown-submenu"
          link_text <- navbar_link_text(x)
        } else {
          menu_class <- "dropdown"
          link_text <- navbar_link_text(x, " ", tags$span(class = "caret"))
        }

        submenuLinks <- navbar_links_tags(x$menu, depth = depth + 1L)

        tags$li(class = menu_class,
                tags$a(
                  href = "#", class = "dropdown-toggle",
                  `data-toggle` = "dropdown", role = "button",
                  `aria-expanded` = "false", link_text),
                tags$ul(class = "dropdown-menu", role = "menu", submenuLinks)
        )

      } else if (!is.null(x$text) && grepl("^\\s*-{3,}\\s*$", x$text)) {

        # divider
        tags$li(class = "divider")

      } else if (!is.null(x$text) && is.null(x$href)) {

        # header
        tags$li(class = "dropdown-header", x$text)

      } else {

        # standard menu item
        textTags <- navbar_link_text(x)
        tags$li(tags$a(href = x$href, textTags))
      }
    })
    tagList(tags)
  } else {
    tagList()
  }
}

navbar_link_text <- function(x, ...) {

  if (!is.null(x$icon)) {
    # find the iconset
    split <- strsplit(x$icon, "-")
    if (length(split[[1]]) > 1)
      iconset <- split[[1]][[1]]
    else
      iconset <- ""
    # check if a full class is passed for fontawesome = V5
    # Add fa deprecated fa prefix otherwise = V4 compatibility
    # https://github.com/rstudio/rmarkdown/issues/1554
    class = if (grepl("^fa\\w? fa", iconset)) {
      # Fontawesome 5 - full new prefix + name must be passed
      # if old fa prefix is passed - keep it for compatibility
      x$icon
    } else if (iconset == "fa") {
      # Fontawesome 4 compatibility - Add deprecated fa prefix
      paste("fa", x$icon)
    } else {
      # Other Icon sets
      paste(iconset, x$icon)
    }
    tagList(tags$span(class = class), " ", x$text, ...)
  }
  else
    tagList(x$text, ...)
}

