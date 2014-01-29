#' Convert to an HTML document
#'
#' Format for converting from R Markdown to an HTML document.
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param fig.width Default width (in inches) for figures
#' @param fig.height Default width (in inches) for figures
#' @param fig.caption \code{TRUE} to render figures with captions
#' @param theme Visual theme ("default", "cerulean", "journal", "flatly",
#'   "readable", "spacelab", "united", "yeti", or "cosmo").
#'   Pass \code{NULL} for no theme (in which case you want to pass some custom
#'   CSS using the \code{css} parameter)
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "pygments", "kate", "monochrome", "espresso", "zenburn",
#'   "haddock", and "tango". Pass \code{NULL} to prevent syntax highlighting.
#' @param mathjax Include mathjax. The "default" option uses an https URL from
#'   the official MathJax CDN. You can pass an alternate URL or pass \code{NULL}
#'   to exclude MathJax entirely.
#' @param css One or more css files to include
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link[pandoc:include_options]{include_options}}
#'   function).
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
                          implicit.figures = FALSE,
                          theme = "default",
                          highlight = "default",
                          mathjax = "default",
                          css = NULL,
                          includes = NULL,
                          pandoc.args = NULL) {

  # knitr options and hooks
  knitr <- knitr_options(
    opts_chunk = list(dev = 'png',
                      dpi = 96,
                      fig.width = fig.width,
                      fig.height = fig.height)
  )

  # base pandoc options for all HTML output
  args <- c("--smart", "--self-contained")

  # table of contents
  args <- c(args, pandoc::toc_options(toc, toc.depth))

  # template path and assets
  args <- c(args,
            pandoc::template_options(pandoc_template("html/default.html")))

  # theme
  if (!is.null(theme)) {
    theme <- match.arg(theme, themes())
    if (identical(theme, "default"))
      theme <- "bootstrap"
    args <- c(args, "--variable", paste("theme:", theme, sep=""))
  }

  # highlighting
  if (is.null(highlight)) {
    args <- c(args, "--no-highlight")
  } else {
    highlight <- match.arg(highlight, highlighters())
    if (identical(highlight, "default")) {
      args <- c(args, "--no-highlight")
      args <- c(args, "--variable", "highlightjs")
    }
    else {
      args <- c(args, "--highlight-style", highlight)
    }
  }

  # mathjax
  if (!is.null(mathjax)) {
    if (identical(mathjax, "default"))
      mathjax <- default_mathjax()
    args <- c(args, "--mathjax")
    args <- c(args, "--variable", paste("mathjax-url:", mathjax, sep=""))
  }

  # additional css
  for (css_file in css)
    args <- c(args, "--css", css_file)

  # content includes
  args <- c(args, includes)

  # pandoc args
  args <- c(args, pandoc.args)

  # return format
  output_format(
    knitr = knitr,
    pandoc = pandoc_options(to = "html",
                            from = from_rmarkdown(implicit.figures),
                            args = args)
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

default_mathjax <- function() {
  paste("https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js",
        "?config=TeX-AMS-MML_HTMLorMML", sep="")
}
