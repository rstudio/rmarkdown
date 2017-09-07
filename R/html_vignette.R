#' Convert to an HTML vignette.
#'
#' A HTML vignette is a lightweight alternative to \code{\link{html_document}}
#' suitable for inclusion in packages to be released to CRAN. It reduces the
#' size of a basic vignette from 100k to around 10k.
#'
#' Compared to \code{html_document}, it:
#'
#' \itemize{
#'   \item never uses retina figures
#'   \item never uses a theme
#'   \item has a smaller default figure size
#'   \item uses a custom css stylesheet
#'   \item uses a custom highlight scheme
#'  }
#'
#' @details
#'
#' See the \href{http://rmarkdown.rstudio.com/package_vignette_format.html}{online
#' documentation} for additional details on using the \code{html_vignette} format.
#'
#' @inheritParams html_document
#' @param ... Additional arguments passed to \code{\link{html_document}}. Please
#'   note that \code{theme}, \code{fig_retina} and \code{highlight} are hard
#'   coded. Setting any of those will yield an error.
#' @param readme Use this vignette as the package README.md file (i.e. render
#'   it as README.md to the package root). Note that if there are image files
#'   within your vignette you should be sure to add README_files to .Rbuildignore
#' @return R Markdown output format to pass to \code{\link{render}}
#' @export
html_vignette <- function(fig_width = 3,
                          fig_height = 3,
                          dev = 'png',
                          df_print = "default",
                          css = NULL,
                          keep_md = FALSE,
                          readme = FALSE,
                          ...) {

  if (is.null(css)) {
    css <- system.file("rmarkdown", "templates", "html_vignette" ,"resources",
      "vignette.css", package = "rmarkdown")
  }

  pre_knit <- function(input, ...) {
    if (readme) {
      rmarkdown::render(input,
                        output_format = "github_document",
                        output_options = list(html_preview = FALSE),
                        output_file = "README.md",
                        output_dir = dirname(dirname(input)),
                        quiet = TRUE)
    }
  }

  output_format(
    knitr = NULL,
    pandoc = NULL,
    df_print = df_print,
    pre_knit = pre_knit,
    keep_md = keep_md,
    base_format = html_document(fig_width = fig_width,
                                fig_height = fig_height,
                                dev = dev,
                                fig_retina = NULL,
                                css = css,
                                theme = NULL,
                                highlight = "pygments",
                                ...)
  )
}
