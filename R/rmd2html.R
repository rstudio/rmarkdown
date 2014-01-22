#' Convert R Markdown to HTML
#'
#' Converts an R Markdown (Rmd) file to HTML
#'
#' @param input Input Rmd document
#' @param output Target output file (defaults to <input>.html if not specified)
#' @param options List of HTML rendering options created by calling
#'   \code{\link{htmlOptions}}
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
                     output = NULL,
                     options = htmlOptions(),
                     envir = parent.frame(),
                     quiet = FALSE,
                     encoding = getOption("encoding")) {

  # knitr options
  knitrRenderHTML("html", 7, 7)

  # call pandoc
  rmd2pandoc(input, "html", output, options, envir, quiet, encoding)
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
#' @param include.header One or more files with HTML content to be included
#'   within the HTML \code{head} tag.
#' @param include.before One or more files with HTML content to be included
#'   before the document body.
#' @param include.after One or more files with HTML content to be included after
#'   the document body.
#'
#' @details Paths for resources referenced from the \code{css},
#'   \code{include.header}, \code{include.before}, and \code{include.after}
#'   parameters are resolved relative to the directory of the input document.
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
                        include.header = NULL,
                        include.before = NULL,
                        include.after = NULL) {
  structure(list(toc = toc,
                 toc.depth = toc.depth,
                 bootstrap = bootstrap,
                 highlight = highlight,
                 mathjax = mathjax,
                 css = css,
                 include.header = include.header,
                 include.before = include.before,
                 include.after = include.after),
            class = "htmlOptions")
}


#' @rdname htmlOptions
#' @export
mathjaxURL <- function() {
  paste0("https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js",
         "?config=TeX-AMS-MML_HTMLorMML")
}


#' @S3method pandocOptions htmlOptions
pandocOptions.htmlOptions <- function(htmlOptions) {

  # base options for all HTML output
  options <- c("--smart", "--self-contained")

  # table of contents
  options <- c(options, pandocTableOfContentsOptions(htmlOptions))

  # template path and assets
  options <- c(options, pandocTemplateOptions("html/default.html"))

  # bootstrap
  if (htmlOptions$bootstrap)
    options <- c(options, "--variable", "bootstrap")

  # highlighting
  if (is.null(htmlOptions$highlight)) {
    options <- c(options, "--no-highlight")
  }
  else if (identical(htmlOptions$highlight, "default")) {
    options <- c(options, "--no-highlight")
    options <- c(options, "--variable", "highlightjs")
  }
  else {
    options <- c(options, "--highlight-style", htmlOptions$highlight)
  }

  # mathjax
  if (!is.null(htmlOptions$mathjax)) {
    options <- c(options, "--mathjax")
    options <- c(options,
                 "--variable", paste0("mathjax-url:", htmlOptions$mathjax))
  }

  # additional css
  for (css in htmlOptions$css)
    options <- c(options, "--css", css)

  # content includes
  options <- c(options, pandocIncludeOptions(htmlOptions))

  options
}
