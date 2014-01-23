#' Convert R Markdown to HTML
#'
#' Converts an R Markdown (Rmd) file to HTML
#'
#' @param input Input Rmd document
#' @param options Character vector of pandoc options created by calling
#'   \code{htmlOptions}
#' @param output Target output file (defaults to <input>.html if not specified)
#' @param envir The environment in which the code chunks are to be evaluated
#'   (can use \code{\link{new.env}()} to guarantee an empty new environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @export
rmd2html <- function(input,
                     options = htmlOptions(),
                     output = NULL,
                     envir = parent.frame(),
                     quiet = FALSE,
                     encoding = getOption("encoding")) {

  # knitr options
  knitrRenderHTML("html", 700, 600)

  # call pandoc
  rmd2pandoc(input, "html", options, output, envir, quiet, encoding)
}


#' @rdname knitrRender
#' @export
knitrRenderHTML <- function(format, out.width, out.height) {

  # inherit defaults
  knitrRender(format)

  # graphics device
  knitr::opts_chunk$set(dev = 'png',
                        out.width = out.width,
                        out.height = out.height)
}


#' Options for HTML Conversion
#'
#' Define the options for converting R Markdown to HTML.
#'
#' @param \dots Command line options to pass to pandoc
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param theme Visual theme ("default", "cerulean", or "slate"). Pass
#' \code{NULL} for no theme (in which case you want to pass some custom
#' CSS using the \code{css} parameter)
#' @param highlight Style for syntax highlighting. Options are default,
#'   pygments, kate, monochrome, espresso, zenburn, haddock, and tango. Pass
#'   \code{NULL} to prevent syntax highlighting.
#' @param mathjax Include mathjax from the specified URL. Pass \code{NULL} to
#'   not include mathjax.
#' @param css One or more css files to include
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link{includeOptions}} function).
#'
#' @return A character vector of HTML options that can be passed to
#'   \code{\link{rmd2html}}.
#'
#' @export
htmlOptions <- function(...,
                        toc = FALSE,
                        toc.depth = 3,
                        theme = "default",
                        highlight = "default",
                        mathjax = mathjaxURL(),
                        css = NULL,
                        includes = NULL) {

  # base options for all HTML output
  options <- c("--smart", "--self-contained")

  # table of contents
  options <- c(options, tableOfContentsOptions(toc, toc.depth))

  # template path and assets
  options <- c(options, templateOptions(pandocTemplate("html/default.html")))

  # theme
  if (!is.null(theme)) {
    if (identical(theme, "default"))
      theme <- "bootstrap"
    options <- c(options, "--variable", paste0("theme:", theme))
  }

  # highlighting
  if (is.null(highlight)) {
    options <- c(options, "--no-highlight")
  }
  else if (identical(highlight, "default")) {
    options <- c(options, "--no-highlight")
    options <- c(options, "--variable", "highlightjs")
  }
  else {
    options <- c(options, "--highlight-style", highlight)
  }

  # mathjax
  if (!is.null(mathjax)) {
    options <- c(options, "--mathjax")
    options <- c(options, "--variable", paste0("mathjax-url:", mathjax))
  }

  # additional css
  for (cssFile in css)
    options <- c(options, "--css", cssFile)

  # content includes
  options <- c(options, includes)

  # dots
  options <- c(options, as.character(list(...)))

  options
}

#' @rdname htmlOptions
#' @export
mathjaxURL <- function() {
  paste0("https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js",
         "?config=TeX-AMS-MML_HTMLorMML")
}
