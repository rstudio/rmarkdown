#' Convert R Markdown to Beamer
#'
#' Converts the input file to Beamer using pandoc. If the input requires
#' knitting then \code{\link[knitr:knit]{knit}} is called prior to pandoc.
#'
#' @param input Input file (Rmd or plain markdown)
#' @param options Character vector of pandoc options created by calling
#'   \code{\link{beamerOptions}}
#' @param output Target output file (defaults to <input>.pdf if not specified)
#' @param envir The environment in which the code chunks are to be evaluated
#'   during knitting (can use \code{\link{new.env}()} to guarantee an empty new
#'   environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @details
#'
#' For more information on markdown syntax for presentations see
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
#' slide shows with pandoc}.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. Metadata can
#' also be provided to enable the use of footnotes and bibliographies.
#' For more details see the documentation on R Markdown
#' \link[=rmdMetadata]{metadata} and \link[=rmdCitations]{citations}.
#'
#' In addition to the options specified by \code{\link{beamerOptions}}, many
#' other aspects of the LaTeX template used to create Beamer output can be
#' customized using metadata. For example:
#'
#' \tabular{l}{
#' \code{---} \cr
#' \code{title: "Crop Analysis Q3 2013"} \cr
#' \code{theme: AnnArbor} \cr
#' \code{colortheme: dolphin} \cr
#' \code{---}
#' }
#'
#' Available metadata variables include:
#'
#' \describe{
#'    \item{\code{lang}}{Document language code}
#'    \item{\code{theme}}{Beamer theme (e.g. "AnnArbor")}
#'    \item{\code{colortheme}}{Beamer color theme (e.g. "dolphin")}
#'    \item{\code{fonttheme}}{Beamer font theme (e.g. "structurebold")}
#'    \item{\code{biblio-style}}{LaTeX bibliography style (used with \code{natbib} option)}
#'    \item{\code{biblio-files}}{Bibliography files to use in LaTeX (used with \code{natbib} or \code{biblatex} options)}
#' }
#'
#' @seealso \code{\link[knitr:knit]{knit}}, \code{\link{beamerOptions}}
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' # simple invocation
#' rmd2beamer("pres.Rmd")
#'
#' # specify an option for incremental rendering
#' rmd2beamer("pres.Rmd", beamerOptions(incremental = TRUE))
#' }
#'
#' @export
rmd2beamer <- function(input,
                       options = beamerOptions(),
                       output = NULL,
                       envir = parent.frame(),
                       quiet = FALSE,
                       encoding = getOption("encoding")) {

  # knitr rendering
  if (knitRequired(input))
    knitrRenderPdf("beamer", 4.5, 3.5)

  # call pandoc
  rmd2pandoc(input, "beamer", options, output, envir, quiet, encoding)
}


#' Options for Beamer Conversion
#'
#' Define the options for converting R Markdown to Beamer
#'
#' @param \dots Command line options to pass to pandoc
#' @param toc \code{TRUE} to include a table of contents in the output (only
#'   level 1 headers will be included in the table of contents).
#' @param slide.level The heading level which defines indvidual slides. By
#'   default this is level 2, which allows level 1 headers to be used to define
#'   sections of the presentation.
#' @param incremental \code{TRUE} to render slide bullets incrementally. Note
#'   that if you want to reverse the default incremental behavior for an
#'   individual bullet you can preceded it with \code{>}. For example:
#'   \emph{\code{> - Bullet Text}}
#' @param highlight Syntax highlighting style (see \code{\link{highlighter}}).
#'   Pass \code{NULL} to prevent syntax highlighting.
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link[pandoc:includeOptions]{includeOptions}} function).
#'
#' @return A character vector of options that can be passed to
#'   \code{\link{rmd2beamer}}.
#'
#' @seealso \code{\link{rmd2beamer}}
#'
#' @export
beamerOptions <- function(...,
                          toc = FALSE,
                          slide.level = 2,
                          incremental = FALSE,
                          highlight = highlighter(),
                          includes = NULL) {

  # base options for all beamer output
  options <- c()

  # table of contents
  if (toc)
    options <- c(options, "--table-of-contents")

  # slide level
  options <- c(options,
               "--slide-level",
               as.character(slide.level))

  # incremental
  if (incremental)
    options <- c(options, "--incremental")

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight)
  options <- c(options, pandoc::highlightOptions(highlight))

  # content includes
  options <- c(options, includes)

  # dots
  options <- c(options, as.character(list(...)))

  options
}



