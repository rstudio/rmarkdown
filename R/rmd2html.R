#' Convert R Markdown to HTML
#'
#' Converts an R Markdown (Rmd) file to HTML
#'
#' @param input Input Rmd document
#' @param options List of HTML rendering options created by calling
#'   \code{\link{htmlOptions}}
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
  knitrRenderHTML("html", 7, 7)

  # call pandoc
  rmd2pandoc(input, "html", options, output, envir, quiet, encoding)
}


#' @rdname knitrRender
#' @export
knitrRenderHTML <- function(format, fig.width, fig.height) {

  # inherit defaults
  knitrRender(format)

  # graphics device
  knitr::opts_chunk$set(dev = 'png',
                        fig.width = fig.width,
                        fig.height = fig.height)
}


#' Options for HTML Conversion
#'
#' Define the options for converting R Markdown to HTML.
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param bootstrap \code{TRUE} to style the document using
#'   \href{http://getbootstrap.com}{Bootstrap}. If you pass \code{FALSE} you can
#'   provide your own styles using the \code{css} and/or \code{include.header}
#'   parameters.
#' @param highlight Style for syntax highlighting. Options are default,
#'   pygments, kate, monochrome, espresso, zenburn, haddock, and tango. Pass
#'   \code{NULL} to prevent syntax highlighting.
#' @param mathjax Include mathjax from the specified URL. Pass \code{NULL} to
#'   not include mathjax.
#' @param css One or more css files to include
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link{pandocIncludeOptions}} function).
#'
#' @return A list of HTML options that can be passed to \code{\link{rmd2html}}.
#'
#' @export
htmlOptions <- function(toc = FALSE,
                        toc.depth = 3,
                        bootstrap = TRUE,
                        highlight = "default",
                        mathjax = mathjaxURL(),
                        css = NULL,
                        includes = NULL) {
  structure(list(toc = toc,
                 toc.depth = toc.depth,
                 bootstrap = bootstrap,
                 highlight = highlight,
                 mathjax = mathjax,
                 css = css,
                 includes = includes),
            class = "htmlOptions")
}


#' @rdname htmlOptions
#' @export
mathjaxURL <- function() {
  paste0("https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js",
         "?config=TeX-AMS-MML_HTMLorMML")
}


#' @S3method pandocOptions htmlOptions
pandocOptions.htmlOptions <- function(options) {

  # base options for all HTML output
  args <- c("--smart", "--self-contained")

  # table of contents
  args <- c(args, pandocTableOfContentsOptions(options))

  # template path and assets
  args <- c(args, pandocTemplateOptions(pandocTemplate("html/default.html")))

  # bootstrap
  if (options$bootstrap)
    args <- c(args, "--variable", "bootstrap")

  # highlighting
  if (is.null(options$highlight)) {
    args <- c(args, "--no-highlight")
  }
  else if (identical(options$highlight, "default")) {
    args <- c(args, "--no-highlight")
    args <- c(args, "--variable", "highlightjs")
  }
  else {
    args <- c(args, "--highlight-style", options$highlight)
  }

  # mathjax
  if (!is.null(options$mathjax)) {
    args <- c(args, "--mathjax")
    args <- c(args, "--variable", paste0("mathjax-url:", options$mathjax))
  }

  # additional css
  for (css in options$css)
    args <- c(args, "--css", css)

  # content includes
  args <- c(args, pandocOptions(options$includes))

  args
}
