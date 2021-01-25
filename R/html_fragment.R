#' Convert to an HTML fragment.
#'
#' An html fragment is suitable for inclusion into an external html page. See
#' \code{\link{html_document}} for full details - this is a minor variation that
#' assumes you will include the output into an existing document (e.g. a blog
#' post).
#'
#' See the \href{https://bookdown.org/yihui/rmarkdown/html-document.html}{online
#' documentation} for additional details on using the \code{html_fragment}
#' format.
#' @param mathjax \code{TRUE} to convert $ and $$ math blocks into MathJax
#'   compatible output. Note that you'll still need to ensure that the page
#'   where the fragment is included loads the required MathJax scripts.
#' @inheritParams html_document
#' @param ... Additional arguments passed to \code{\link{html_document}}
#' @return R Markdown output format to pass to \code{\link{render}}
#' @export
html_fragment <- function(number_sections = FALSE,
                          section_divs = TRUE,
                          fig_width = 7,
                          fig_height = 5,
                          fig_retina = 2,
                          fig_caption = TRUE,
                          dev = 'png',
                          df_print = "default",
                          mathjax = TRUE,
                          includes = NULL,
                          keep_md = FALSE,
                          md_extensions = NULL,
                          pandoc_args = NULL,
                          ...) {

  if (mathjax)
    pandoc_args <- c(pandoc_args, "--mathjax")

  html_document(
    number_sections = number_sections, fig_width = fig_width,
    fig_height = fig_height, fig_retina = fig_retina, fig_caption = fig_caption,
    dev = dev, df_print = df_print, keep_md = keep_md,
    md_extensions = md_extensions, pandoc_args = pandoc_args, includes = includes,
    mathjax = NULL, section_divs = section_divs, highlight = NULL, theme = NULL, ...,
    template = pkg_file("rmd/fragment/default.html")
  )
}
