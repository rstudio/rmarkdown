#' Convert R Markdown to Beamer
#'
#' Converts an R Markdown (Rmd) file to a Beamer Presentation PDF. The document is \code{\link[knitr:knit]{knit}} and then converted to PDF using \href{http://johnmacfarlane.net/pandoc/index.html}{pandoc}.
#'
#' @param input input Rmd document
#' @param options Character vector of pandoc options created by calling
#'   \code{\link{beamerOptions}}
#' @param output Target output file (defaults to <input>.pdf if not specified)
#' @param envir The environment in which the code chunks are to be evaluated
#'   (can use \code{\link{new.env}()} to guarantee an empty new environment)
#' @param quiet Whether to suppress the progress bar and messages
#' @param encoding The encoding of the input file; see \code{\link{file}}
#'
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned.
#'
#' @section Metadata:
#'  Rmd files include a metadata section (typically located at the top of the file) that include title, author, and date information as well additional variables used to customize document generation. Here is an example metadata section:
#'
#' \tabular{l}{
#' \code{---} \cr
#' \code{title: "Crop Analysis Q3 2013"} \cr
#' \code{author: Martha Smith} \cr
#' \code{date: October 23rd, 2013} \cr
#' \code{theme: Antibes} \cr
#' \code{colortheme: crane} \cr
#' \code{---}
#' }
#'
#' In addition to the options specified by \code{\link{beamerOptions}}, many other aspects of the Beamer template can be customized using metadata. Available variables include:
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
#' @section Citations:
#' R Markdown documents can also include footnotes and citations, with support for a wide variety of bibliography formats and output styles. To define the bibliography and citation styles for a document you add the \code{bibliography} and \code{csl} metadata fields. For example:
#'
#' \tabular{l}{
#' \code{---} \cr
#' \code{title: "Crop Analysis Q3 2013"} \cr
#' \code{bibliography: crop-analysis.bib} \cr
#' \code{csl: chicago-author-date.csl} \cr
#' \code{---}
#' }
#'
#' Note that the referenced bibliography and csl files should be located in the same directory as your R Markdown document.
#'
#' You can find more information on the markdown syntax for citations within the pandoc documentation on \href{http://johnmacfarlane.net/pandoc/README.html#footnotes}{footnotes} and \href{http://johnmacfarlane.net/pandoc/README.html#citations}{citations}.
#'
#' @seealso \code{\link[knitr:knit]{knit}}, \code{\link{beamerOptions}},
#'
#' @export
rmd2beamer <- function(input,
                       options = beamerOptions(),
                       output = NULL,
                       envir = parent.frame(),
                       quiet = FALSE,
                       encoding = getOption("encoding")) {

  # knitr options
  knitrRenderPDF("beamer", 4.5, 3.5)

  # call pandoc
  rmd2pandoc(input, "beamer", options, output, envir, quiet, encoding)
}


#' Options for Beamer conversion
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
#' @param highlight Style for syntax highlighting. Options are default,
#'   pygments, kate, monochrome, espresso, zenburn, haddock, and tango. Pass
#'   \code{NULL} to prevent syntax highlighting.
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
                          highlight = "default",
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
  options <- c(options, pandoc::highlightOptions(highlight))

  # content includes
  options <- c(options, includes)

  # dots
  options <- c(options, as.character(list(...)))

  options
}



