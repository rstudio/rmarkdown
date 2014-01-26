#' Convert R Markdown to HTML
#'
#' Converts the input file to HTML using pandoc. If the input requires knitting
#' then \code{\link[knitr:knit]{knit}} is called prior to pandoc.
#'
#' @param input Input file (Rmd or plain markdown)
#' @param options Character vector of pandoc options created by calling
#'   \code{\link{htmlOptions}}
#' @param output Target output file (defaults to <input>.html if not specified)
#' @param envir The environment in which the code chunks are to be evaluated
#'   during knitting (can use \code{\link{new.env}()} to guarantee an empty new
#'   environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @details R Markdown documents can have optional metadata that is used to
#'   generate a document header that includes the title, author, and date.
#'   Metadata can also be provided to enable the use of footnotes and
#'   bibliographies. For more details see the documentation on
#'   \link[=rmdMetadata]{R Markdown Metadata}.
#'
#' @seealso \code{\link[knitr:knit]{knit}}, \code{\link{htmlOptions}}
#'
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#' rmd2html("input.Rmd")
#'
#' rmd2html("input.Rmd", htmlOptions(toc = TRUE))
#' }
#'
#' @export
rmd2html <- function(input,
                     options = htmlOptions(),
                     output = NULL,
                     envir = parent.frame(),
                     quiet = FALSE,
                     encoding = getOption("encoding")) {

  # knitr rendering
  if (knitRequired(input))
    knitrRenderHtml("html", 7, 7)

  # call pandoc
  rmd2pandoc(input, "html", options, output, envir, quiet, encoding)
}


#' @rdname knitrRender
#' @export
knitrRenderHtml <- function(format, fig.width, fig.height) {

  # inherit defaults
  knitrRender(format)

  # graphics device
  knitr::opts_chunk$set(dev = 'png',
                        dpi = 96,
                        fig.width = fig.width,
                        fig.height = fig.height)
}


#' Options for HTML Conversion
#'
#' Define the options for converting R Markdown to HTML.
#'
#' @param \dots Command line options to pass to pandoc
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param theme Visual theme ("default", "cerulean", or "slate"). Pass
#'   \code{NULL} for no theme (in which case you want to pass some custom CSS
#'   using the \code{css} parameter)
#' @param highlight Syntax highlighting style (see
#'   \code{\link{highlighter}}). Pass \code{NULL} to prevent syntax highlighting.
#' @param mathjax Include mathjax from the specified URL. Pass \code{NULL} to
#'   not include mathjax.
#' @param css One or more css files to include
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link[pandoc:includeOptions]{includeOptions}}
#'   function).
#'
#' @return A character vector of HTML options that can be passed to
#'   \code{\link{rmd2html}}.
#'
#' @seealso \code{\link{rmd2html}}
#'
#' @export
htmlOptions <- function(...,
                        toc = FALSE,
                        toc.depth = 3,
                        theme = c("default", "cerulean", "slate"),
                        highlight = highlighter(),
                        mathjax = mathjaxURL(),
                        css = NULL,
                        includes = NULL) {

  # base options for all HTML output
  options <- c("--smart", "--self-contained")

  # table of contents
  options <- c(options, pandoc::tocOptions(toc, toc.depth))

  # template path and assets
  options <- c(options,
               pandoc::templateOptions(pandocTemplate("html/default.html")))

  # theme
  if (!is.null(theme)) {
    theme <- match.arg(theme)
    if (identical(theme, "default"))
      theme <- "bootstrap"
    options <- c(options, "--variable", paste0("theme:", theme))
  }

  # highlighting
  if (is.null(highlight)) {
    options <- c(options, "--no-highlight")
  } else {
    highlight <- match.arg(highlight)
    if (identical(highlight, "default")) {
      options <- c(options, "--no-highlight")
      options <- c(options, "--variable", "highlightjs")
    }
    else {
      options <- c(options, "--highlight-style", highlight)
    }
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
