#' Convert to a markdown document
#'
#' Format for converting from R Markdown to another variant of markdown (e.g.
#' strict markdown or github flavored markdown)
#'
#' @inheritParams html_document
#'
#' @param variant Markdown variant to produce (defaults to "markdown_strict").
#'   Other valid values are "markdown_github", "markdown_mmd",
#'   markdown_phpextra", or even "markdown" (which produces pandoc markdown).
#'   You can also compose custom markdown variants, see the documentation on
#'   \href{http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html}{pandoc's
#'    markdown} for details.
#'
#' @param fig.retina Scaling to perform for retina displays. Defaults to
#'   \code{NULL} which performs no scaling. A setting of 2 will work for all
#'   widely used retina displays, but will also result in the output of
#'   \code{<img>} tags rather than markdown images due to the need to set the
#'   width of the image explicitly. Note that this only takes effect if you are
#'   using knitr >= 1.5.21.
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
#' render("input.Rmd", md_document())
#'
#' render("input.Rmd", md_document(variant = "github_flavored_markdown"))
#' }
#'
#' @export
md_document <- function(variant = "markdown_strict",
                        toc = FALSE,
                        toc.depth = 3,
                        fig.width = 7,
                        fig.height = 5,
                        fig.retina = NULL,
                        includes = NULL,
                        pandoc.args = NULL) {

  # base pandoc options for all markdown output
  args <- c("--standalone")

  # table of contents
  args <- c(args, pandoc::toc_options(toc, toc.depth))

  # content includes
  args <- c(args, includes)

  # pandoc args
  args <- c(args, pandoc.args)

  # return format
  output_format(
    knitr = knitr_options_html(fig.width, fig.height, fig.retina),
    pandoc = pandoc_options(to = variant,
                            from = from_rmarkdown(),
                            args = args)
  )
}



# Filter to copy mathjax files to the output directory if necessary
filter_html <- function(local_mathjax) {

  function(output.format, output.file, input) {

    if (local_mathjax) {
      output_dir <- dirname(output.file)
      mathjax_stage_dir <- file.path(output_dir, "m")
      mathjax_dir <- file.path(output_dir, "mathjax")
      if (!file.exists(mathjax_dir) && !file.exists(mathjax_stage_dir)) {
        dir.create(mathjax_dir)
        file.copy(from = file.path(pandoc_template("h"), "m"),
                  to = output_dir,
                  recursive = TRUE)
        file.rename(from = file.path(output_dir, "m"),
                    to = mathjax_dir)
      }
    }

    output.format
  }

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

html_highlighters <- function() {
  c(highlighters(), "textmate")
}

default_mathjax <- function() {
  paste("https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/",
        mathjax_config(), sep="")
}

local_mathjax <- function() {
  paste("mathjax/", mathjax_config(), sep="")
}

mathjax_config <- function() {
  "MathJax.js?config=TeX-AMS-MML_HTMLorMML"
}


