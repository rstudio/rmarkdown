#' Convert to an HTML document
#'
#' Format for converting from R Markdown to an HTML document.
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc_depth Depth of headers to include in table of contents
#' @param fig_width Default width (in inches) for figures
#' @param fig_height Default width (in inches) for figures
#' @param fig_retina Scaling to perform for retina displays (defaults to 2,
#'   which currently works for all widely used retina displays). Note that this
#'   only takes effect if you are using knitr >= 1.5.21. Set to \code{NULL} to
#'   prevent retina scaling.
#' @param fig_caption \code{TRUE} to render figures with captions
#' @param smart Produce typographically correct output, converting straight
#'   quotes to curly quotes, --- to em-dashes, -- to en-dashes, and ... to
#'   ellipses.
#' @param self_contained Produce a standalone HTML file with no external
#'   dependencies, using data: URIs to incorporate the contents of linked
#'   scripts, stylesheets, images, and videos. Note that if you specify
#'   "local" for \code{mathjax} then \code{self_contained} is automatically
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
#' @param data_dir Additional directory to resolve relatives paths of templates
#'   and included content against (the directory of the input file is used by
#'   default).
#' @param pandoc_args Additional command line options to pass to pandoc
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
#'      style will resolve to "pygments" and the "textmate" highlighting
#'      style is not available
#'   }
#'   \item{MathJax is automatically disabled if \code{self_contained} is
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
                          toc_depth = 3,
                          fig_width = 7,
                          fig_height = 5,
                          fig_retina = 2,
                          fig_caption = FALSE,
                          smart = TRUE,
                          self_contained = TRUE,
                          theme = "default",
                          highlight = "default",
                          mathjax = "default",
                          template = NULL,
                          css = NULL,
                          includes = NULL,
                          data_dir = NULL,
                          pandoc_args = NULL) {

  # interplay between arguments
  self_contained <- reconcile_self_contained(self_contained, mathjax)

  # build pandoc args
  args <- c("--standalone")

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  # self contained document
  if (self_contained)
    args <- c(args, "--self-contained")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # template path and assets
  if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))
  else
    args <- c(args, "--template",
              pandoc_path_arg(rmarkdown_system_file("rmd/h/default.html")))

  # additional css
  for (css_file in css)
    args <- c(args, "--css", pandoc_path_arg(css_file))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # data dir
  if (!is.null(data_dir))
    args <- c(args, "--data-dir", pandoc_path_arg(data_dir))

  # pandoc args
  args <- c(args, pandoc_args)

  # build a filter we'll use for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  filter <- function(output_format, files_dir, input_lines) {

    # extra args
    args <- c()

    # determine bootstrap path (copy supporting if not self contained)
    if (!is.null(theme)) {

      theme <- match.arg(theme, themes())
      if (identical(theme, "default"))
        theme <- "bootstrap"

      bootstrap_path <- rmarkdown_system_file("rmd/h/bootstrap-3.0.3")
      if (!self_contained)
        bootstrap_path <- render_supporting_files(bootstrap_path, files_dir)

      # form path to theme and add theme variable
      theme_path <- paste(bootstrap_path,
                          "/css/",
                          theme,
                          ".min.css",
                          sep="")
      theme_path <- pandoc_path_arg(theme_path)
      args <- c(args, "--variable", paste("theme:", theme_path, sep=""))
    }

    # highlight
    args <- c(args, pandoc_html_highlight_args(highlight,
                                               template,
                                               self_contained,
                                               files_dir))

    # mathjax
    args <- c(args, pandoc_mathjax_args(mathjax,
                                        template,
                                        self_contained,
                                        files_dir))

    # return format with ammended args
    output_format$pandoc$args <- c(output_format$pandoc$args, args)
    output_format
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina),
    pandoc = pandoc_options(to = "html5",
                            from = from_rmarkdown(fig_caption),
                            args = args),
    clean_supporting = self_contained,
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
knitr_options_html <- function(fig_width, fig_height, fig_retina) {
  knitr_options(
    opts_chunk = list(dev = 'png',
                      dpi = 96,
                      fig.width = fig_width,
                      fig.height = fig_height,
                      fig.retina = fig_retina)
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
