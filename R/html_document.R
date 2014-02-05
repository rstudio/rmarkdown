#' Convert to an HTML document
#'
#' Format for converting from R Markdown to an HTML document.
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param fig.width Default width (in inches) for figures
#' @param fig.height Default width (in inches) for figures
#' @param fig.retina Scaling to perform for retina displays (defaults to 2,
#'   which currently works for all widely used retina displays). Note that this
#'   only takes effect if you are using knitr >= 1.5.21. Set to \code{NULL} to
#'   prevent retina scaling.
#' @param fig.caption \code{TRUE} to render figures with captions
#' @param smart Produce typographically correct output, converting straight
#'   quotes to curly quotes, --- to em-dashes, -- to en-dashes, and ... to
#'   ellipses.
#' @param self.contained Produce a standalone HTML file with no external
#'   dependencies, using data: URIs to incorporate the contents of linked
#'   scripts, stylesheets, images, and videos. Note that if you specify
#'   "local" for \code{mathjax} then \code{self.contained} is automatically
#'   set to \code{FALSE}.
#' @param theme Visual theme ("default", "cerulean", "journal", "flatly",
#'   "readable", "spacelab", "united", "yeti", or "cosmo"). Pass \code{NULL} for
#'   no theme (in this case you can use the \code{css} parameter to add your own
#'   styles).
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "tango", "pygments", "kate", "monochrome", "espresso",
#'   "zenburn", "haddock", and "textmate". Pass \code{NULL} to prevent syntax
#'   highlighting.
#' @param mathjax Include mathjax. The "default" option uses an https URL from
#'   the official MathJax CDN. The "local" option uses a local version of
#'   MathJax (which is copied into the output directory). You can pass an
#'   alternate URL or pass \code{NULL} to exclude MathJax entirely.
#' @param template Pandoc template to use for rendering HTML. See the Templates
#'   section below for more details on templates.
#' @param css One or more css files to include
#' @param includes Named list of additional content to include within the
#'   document (typically created using the \code{\link{includes}} function).
#' @param data.dir Additional directory to resolve relatives paths of templates
#'   and included content against (the directory of the input file is used by
#'   default).
#' @param knitr.options Additional options for knitr. To provide options call
#'   the \code{\link{knitr_options}} function and pass it's result.
#' @param pandoc.args Additional command line options to pass to pandoc
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. Metadata can also
#' be provided to enable the use of footnotes and bibliographies. For more
#' details see the documentation on R Markdown \link[=rmd_metadata]{metadata}
#' and \link[=rmd_citations]{citations}.
#'
#'
#' @section Templates:
#'
#' You can provide a custom HTML template to be used for rendering. The syntax
#' for templates is described in the documentation on
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/templates.html}{pandoc
#' templates}.
#'
#' Note however that if you choose to provide your own HTML template then
#' several aspects of HTML document rendering will behave differently:
#'
#' \itemize{
#'   \item{The \code{theme} parameter does not work (you can still
#'      provide styles using the \code{css} parameter).
#'   }
#'   \item{For the \code{highlight} parameter, the default highlighting
#'      style will resolve to "tango" and the "textmate" highlighting
#'      style is not available
#'   }
#'   \item{MathJax is automatically disabled if \code{self.contained} is
#'      \code{TRUE} (these two options can't be used together in normal
#'      pandoc templates).
#'   }
#' }
#'
#' Due to the above restrictions, you might consider using the \code{includes}
#' parameter as an alternative to providing a fully custom template.
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' render("input.Rmd", html_document())
#'
#' render("input.Rmd", html_document(toc = TRUE))
#' }
#'
#' @export
html_document <- function(toc = FALSE,
                          toc.depth = 3,
                          fig.width = 7,
                          fig.height = 5,
                          fig.retina = 2,
                          fig.caption = FALSE,
                          smart = TRUE,
                          self.contained = TRUE,
                          theme = "default",
                          highlight = "default",
                          mathjax = "default",
                          template = NULL,
                          css = NULL,
                          includes = NULL,
                          data.dir = NULL,
                          knitr.options = NULL,
                          pandoc.args = NULL) {

  # interplay between arguments

  # local mathjax forces !self.contained
  if (identical(mathjax, "local"))
    self.contained <- FALSE

  # build pandoc args
  args <- c("--standalone")

  # list we'll use within our filter to add arguments that depend
  # on knowledge of the input file
  dynamic_args <- list()

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  # self contained document
  if (self.contained)
    args <- c(args, "--self-contained")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc.depth))

  # template path and assets
  if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))
  else
    args <- c(args, "--template",
              pandoc_path_arg(rmarkdown_system_file("rmd/h/default.html")))

  # theme
  if (!is.null(theme)) {
    theme <- match.arg(theme, themes())
    if (identical(theme, "default"))
      theme <- "bootstrap"
    dynamic_args$theme <- theme
  }

  # highlighting
  if (is.null(highlight)) {
    args <- c(args, "--no-highlight")
  }
  else if (!is.null(template)) {
    if (identical(highlight, "default"))
      highlight <- "tango"
    args <- c(args, "--highlight-style", highlight)
  }
  else {
    highlight <- match.arg(highlight, html_highlighters())
    if (highlight %in% c("default", "textmate")) {
      dynamic_args$highlight <- highlight
    }
    else {
      args <- c(args, "--highlight-style", highlight)
    }
  }

  # mathjax
  allow_mathjax <- is.null(template) || !self.contained
  if (allow_mathjax && !is.null(mathjax)) {
    dynamic_args$mathjax <- mathjax
  }

  # additional css
  for (css_file in css)
    args <- c(args, "--css", pandoc_path_arg(css_file))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # data dir
  if (!is.null(data.dir))
    args <- c(args, "--data-dir", pandoc_path_arg(data.dir))

  # pandoc args
  args <- c(args, pandoc.args)

  # build a filter we'll use for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  filter <- function(output.format, files.dir, input.lines) {

    # extra args
    args <- c()

    # determine bootstrap path (copy supporting if not self contained)
    if (!is.null(dynamic_args$theme)) {
      bootstrap_path <- rmarkdown_system_file("rmd/h/bootstrap")
      if (!self.contained)
        bootstrap_path <- render_supporting_files(bootstrap_path, files.dir)

      # form path to theme and add theme variable
      theme_path <- paste(bootstrap_path,
                          "/css/",
                          dynamic_args$theme,
                          ".min.css",
                          sep="")
      theme_path <- pandoc_path_arg(theme_path)
      args <- c(args, "--variable", paste("theme:", theme_path, sep=""))
    }

    # highlight
    if (!is.null(dynamic_args$highlight)) {
      highlight_path <- rmarkdown_system_file("rmd/h/highlight")
      if (!self.contained)
        highlight_path <- render_supporting_files(highlight_path, files.dir)
      highlight_path <- pandoc_path_arg(highlight_path)
      args <- c(args, "--no-highlight")
      args <- c(args,
                "--variable", paste("highlightjs=", highlight_path, sep=""))
      if (identical(dynamic_args$highlight, "textmate")) {
        args <- c(args,
                  "--variable",
                  paste("highlightjs-theme=", dynamic_args$highlight, sep=""))
      }
    }

    # mathjax
    if (!is.null(dynamic_args$mathjax)) {
      if (identical(dynamic_args$mathjax, "default"))
        mathjax_url <- default_mathjax()
      else if (identical(dynamic_args$mathjax, "local")) {
        mathjax_path <- rmarkdown_system_file("rmd/h/m")
        mathjax_path <- render_supporting_files(mathjax_path,
                                                files.dir,
                                                "mathjax")
        mathjax_path <- pandoc_path_arg(mathjax_path)
        mathjax_url <- paste(mathjax_path, "/", mathjax_config(), sep = "")
      }
      args <- c(args, "--mathjax")
      args <- c(args, "--variable", paste("mathjax-url:", mathjax_url, sep=""))
    }

    # return format with ammended args
    output.format$pandoc$args <- c(output.format$pandoc$args, args)
    output.format
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig.width, fig.height, fig.retina),
    pandoc = pandoc_options(to = "html",
                            from = from_rmarkdown(fig.caption),
                            args = args),
    clean.supporting = self.contained,
    filter = filter
  )
}


#' Knitr options for an HTML output format
#'
#' Define knitr options for an R Markdown output format that creates
#' HTML output.
#'
#' @inheritParams html_document
#'
#' @return An list that can be passed as the \code{knitr} argument of the
#'   \code{\link{output_format}} function.
#'
#' @seealso \link{knitr_options}, \link{output_format}
#'
#' @export
knitr_options_html <- function(fig.width, fig.height, fig.retina) {
  knitr_options(
    opts_chunk = list(dev = 'png',
                      dpi = 96,
                      fig.width = fig.width,
                      fig.height = fig.height,
                      fig.retina = fig.retina)
  )
}

themes <- function() {
  c("default",
    "cerulean",
    "journal",
    "flatly",
    "readable",
    "spacelab",
    "united",
    "yeti",
    "cosmo")
}

html_highlighters <- function() {
  c(highlighters(), "textmate")
}

default_mathjax <- function() {
  paste("https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/",
        mathjax_config(), sep="")
}

mathjax_config <- function() {
  "MathJax.js?config=TeX-AMS-MML_HTMLorMML"
}
