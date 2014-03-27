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
#' @param preserve_yaml Preserve YAML front matter in final document.
#'
#' @param fig_retina Scaling to perform for retina displays. Defaults to
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
                        preserve_yaml = FALSE,
                        toc = FALSE,
                        toc_depth = 3,
                        fig_width = 7,
                        fig_height = 5,
                        fig_retina = NULL,
                        includes = NULL,
                        data_dir = NULL,
                        pandoc_args = NULL) {

  # base pandoc options for all markdown output
  args <- c("--standalone")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # data dir
  if (!is.null(data_dir))
    args <- c(args, "--data-dir", pandoc_path_arg(data_dir))

  # pandoc args
  args <- c(args, pandoc_args)

  # add post_processor for yaml preservation
  if (preserve_yaml) {
    post_processor <- function(input_file, output_file, verbose) {
      input_lines <- readLines(input_file, warn = FALSE)
      partitioned <- partition_yaml_front_matter(input_lines)
      if (!is.null(partitioned$front_matter)) {
        output_lines <- c(partitioned$front_matter,
                          "",
                          readLines(output_file, warn = FALSE))
        writeLines(output_lines, output_file, useBytes = TRUE)
      }
    }
  } else {
    post_processor <- NULL
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina),
    pandoc = pandoc_options(to = variant,
                            from = from_rmarkdown(),
                            args = args),
    post_processor = post_processor
  )
}
