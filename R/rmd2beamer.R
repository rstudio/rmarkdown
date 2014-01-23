#' Convert R Markdown to Beamer
#'
#' Converts an R Markdown (Rmd) file to a Beamer Presentation PDF
#'
#' @param input input Rmd document
#' @param output Target output file (defaults to <input>.pdf if not specified)
#' @param options List of Beamer rendering options created by calling
#'   \code{beamerOptions}
#' @param envir The environment in which the code chunks are to be evaluated
#'   (can use \code{\link{new.env}()} to guarantee an empty new environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @export
rmd2beamer <- function(input,
                       output = NULL,
                       options = beamerOptions(),
                       envir = parent.frame(),
                       quiet = FALSE,
                       encoding = getOption("encoding")) {

  # knitr options
  knitrRenderPDF("beamer", 4.5, 3.5)

  # call pandoc
  rmd2pandoc(input, "beamer", output, options, envir, quiet, encoding)
}


#' Options for Beamer conversion
#'
#' Define the options for converting R Markdown to Beamer
#'
#' @param toc \code{TRUE} to include a table of contents in the output (only
#'   level 1 headers will be included in the table of contents).
#' @param slide.level The heading level which defines indvidual slides. By
#'   default this is level 2, which allows level 1 headers to be used to define
#'   sections of the presentation.
#' @param incremental \code{TRUE} to render slide bullets incrementally. Note
#'   that if you want to reverse the default incremental behavior for an
#'   individual bullet you can preceded it with \code{>}. For example:
#'   \emph{\code{> - Bullet Text}}
#' @param highlight Style for syntax highlighting. Options are default,
#'   pygments, kate, monochrome, espresso, zenburn, haddock, and tango. Pass
#'   \code{NULL} to prevent syntax highlighting.
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link{pandocIncludeOptions}} function).
#'
#' @return A list of options that can be passed to \code{\link{rmd2beamer}}.
#'
#' @export
beamerOptions <- function(toc = FALSE,
                          slide.level = 2,
                          incremental = FALSE,
                          highlight = "default",
                          includes = NULL) {
  structure(list(toc = toc,
                 slide.level = slide.level,
                 incremental = incremental,
                 highlight = highlight,
                 includes = includes),
            class = "beamerOptions")
}


#' @S3method pandocOptions beamerOptions
pandocOptions.beamerOptions <- function(options) {

  # base options for all beamer output
  args <- c()

  # table of contents
  if (options$toc)
    args <- c(args, "--table-of-contents")

  # slide level
  args <- c(args,
            "--slide-level",
            as.character(options$slide.level))

  # incremental
  if (options$incremental)
    args <- c(args, "--incremental")

  # highlighting
  args <- c(args, pandocHighlightOptions(options))

  # content includes
  args <- c(args, pandocOptions(options$includes))

  args
}



