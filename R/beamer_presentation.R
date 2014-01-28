#' Convert to a Beamer presentation
#'
#' Format for converting from R Markdown to a Beamer presentation.
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
#' @param fig.width Default width (in inches) for figures
#' @param fig.height Default width (in inches) for figures
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "pygments", "kate", "monochrome", "espresso", "zenburn",
#'   "haddock", and "tango". Pass \code{NULL} to prevent syntax highlighting.
#' @param includes Additional content to include within the document (typically
#'   created using the \code{\link[pandoc:include_options]{include_options}}
#'   function).
#' @param \dots Command line options to pass to pandoc
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' Creating Beamer output from R Markdown requires that LaTeX be installed.
#'
#' For more information on markdown syntax for presentations see
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
#' slide shows with pandoc}.
#'
#' When including figures generated from R plots within Beamer slides it's
#' likely you'll need to tweak their sizes for optimal results. You can do this
#' using the \code{fig.width} and \code{fig.height} chunk options. If you find
#' that there is too much space around a plot (common with base graphics) you
#' can apply the pdfcrop filter by adding the \code{crop = TRUE} chunk option.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. Metadata can
#' also be provided to enable the use of footnotes and bibliographies.
#' For more details see the documentation on R Markdown
#' \link[=rmd_metadata]{metadata} and \link[=rmd_citations]{citations}.
#'
#' Many aspects of the LaTeX template used to create Beamer output can be
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
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' # simple invocation
#' render("pres.Rmd", beamer_presentation())
#'
#' # specify an option for incremental rendering
#' render("pres.Rmd", beamer_presentation(incremental = TRUE))
#' }
#'
#' @export
beamer_presentation <- function(...,
                                toc = FALSE,
                                slide.level = 2,
                                incremental = FALSE,
                                fig.width = 4.5,
                                fig.height = 3.5,
                                highlight = "default",
                                includes = NULL) {

  # base pandoc options for all beamer output
  pandoc <- c()

  # table of contents
  if (toc)
    pandoc <- c(pandoc, "--table-of-contents")

  # slide level
  pandoc <- c(pandoc, "--slide-level", as.character(slide.level))

  # incremental
  if (incremental)
    pandoc <- c(pandoc, "--incremental")

  # highlighting
  if (!is.null(highlight))
    highlight <- match.arg(highlight, highlighters())
  pandoc <- c(pandoc, pandoc::highlight_options(highlight))

  # content includes
  pandoc <- c(pandoc, includes)

  # dots
  pandoc <- c(pandoc, as.character(list(...)))

  # return format
  output_format(to = "beamer",
                knitr = knitr_pdf(fig.width, fig.height),
                pandoc = pandoc)
}



