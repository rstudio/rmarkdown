#' Convert to an HTML document
#'
#' Format for converting from R Markdown to an HTML document.
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param fig.width Default width (in inches) for figures
#' @param fig.height Default width (in inches) for figures
#' @param theme Visual theme ("default", "cerulean", or "slate"). Pass
#'   \code{NULL} for no theme (in which case you want to pass some custom CSS
#'   using the \code{css} parameter)
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
#' details see the documentation on R Markdown \link[=rmd_metadata]{metadata} and
#' \link[=rmd_citations]{citations}.
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
                          theme = "default",
                          highlight = "default",
                          mathjax = "default",
                          css = NULL,
                          includes = NULL,
                          pandoc.args = NULL) {

  # knitr options and hooks
  knitr <- list()
  knitr$opts_chunk = list(dev = 'png',
                          dpi = 96,
                          fig.width = fig.width,
                          fig.height = fig.height)

  # base pandoc options for all HTML output
  pandoc <- c("--smart", "--self-contained")

  # table of contents
  pandoc <- c(pandoc, pandoc::toc_options(toc, toc.depth))

  # template path and assets
  pandoc <- c(pandoc,
              pandoc::template_options(pandoc_template("html/default.html")))

  # theme
  if (!is.null(theme)) {
    theme <- match.arg(theme, c("default", "cerulean", "slate"))
    if (identical(theme, "default"))
      theme <- "bootstrap"
    pandoc <- c(pandoc, "--variable", paste("theme:", theme, sep=""))
  }

  # highlighting
  if (is.null(highlight)) {
    pandoc <- c(pandoc, "--no-highlight")
  } else {
    highlight <- match.arg(highlight, highlighters())
    if (identical(highlight, "default")) {
      pandoc <- c(pandoc, "--no-highlight")
      pandoc <- c(pandoc, "--variable", "highlightjs")
    }
    else {
      pandoc <- c(pandoc, "--highlight-style", highlight)
    }
  }

  # mathjax
  if (!is.null(mathjax)) {
    if (identical(mathjax, "default"))
      mathjax <- default_mathjax()
    pandoc <- c(pandoc, "--mathjax")
    pandoc <- c(pandoc, "--variable", paste("mathjax-url:", mathjax, sep=""))
  }

  # additional css
  for (css_file in css)
    pandoc <- c(pandoc, "--css", css_file)

  # content includes
  pandoc <- c(pandoc, includes)

  # pandoc args
  pandoc <- c(pandoc, pandoc.args)

  # return format
  output_format(to = "html",
                knitr = knitr,
                pandoc = pandoc)
}

default_mathjax <- function() {
  paste("https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js",
        "?config=TeX-AMS-MML_HTMLorMML", sep="")
}
