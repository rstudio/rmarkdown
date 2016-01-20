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
#'   You can also compose custom markdown variants, see the
#'   \href{http://pandoc.org/README.html}{pandoc online documentation}
#'   for details.
#'
#' @param preserve_yaml Preserve YAML front matter in final document.
#'
#' @param fig_retina Scaling to perform for retina displays. Defaults to
#'   \code{NULL} which performs no scaling. A setting of 2 will work for all
#'   widely used retina displays, but will also result in the output of
#'   \code{<img>} tags rather than markdown images due to the need to set the
#'   width of the image explicitly.
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' See the \href{http://rmarkdown.rstudio.com/markdown_document_format.html}{online
#' documentation} for additional details on using the \code{md_document} format.
#'
#' R Markdown documents can have optional metadata that is used to generate a
#' document header that includes the title, author, and date. For more details
#' see the documentation on R Markdown \link[=rmd_metadata]{metadata}.
#'
#' R Markdown documents also support citations. You can find more information on
#' the markdown syntax for citations in the
#' \href{http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html}{Bibliographies
#' and Citations} article in the online documentation.
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' render("input.Rmd", md_document())
#'
#' render("input.Rmd", md_document(variant = "markdown_github"))
#' }
#'
#' @export
md_document <- function(variant = "markdown_strict",
                        preserve_yaml = FALSE,
                        toc = FALSE,
                        toc_depth = 3,
                        fig_width = 7,
                        fig_height = 5,
                        fig_retina = NULL,
                        dev = 'png',
                        includes = NULL,
                        md_extensions = NULL,
                        pandoc_args = NULL) {

  # base pandoc options for all markdown output
  args <- c("--standalone")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # pandoc args
  args <- c(args, pandoc_args)

  # add post_processor for yaml preservation
  if (preserve_yaml) {
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
      input_lines <- readLines(input_file, warn = FALSE)
      partitioned <- partition_yaml_front_matter(input_lines)
      if (!is.null(partitioned$front_matter)) {
        output_lines <- c(partitioned$front_matter,
                          "",
                          readLines(output_file, warn = FALSE))
        writeLines(output_lines, output_file, useBytes = TRUE)
      }
      output_file
    }
  } else {
    post_processor <- NULL
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina, FALSE, dev),
    pandoc = pandoc_options(to = variant,
                            from = from_rmarkdown(extensions = md_extensions),
                            args = args),
    clean_supporting = FALSE,
    post_processor = post_processor
  )
}
