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
#'   \item has a smaller default figure size
#'   \item uses a custom css stylesheet
#'  }
#'
#' @details
#'
#' See the \href{http://rmarkdown.rstudio.com/package_vignette_format.html}{online
#' documentation} for additional details on using the \code{html_vignette} format.
#'
#' @inheritParams html_document
#' @param ... Additional arguments passed to \code{\link{html_document}}
#' @return R Markdown output format to pass to \code{\link{render}}
#' @export
html_vignette <- function(fig_width = 3,
                          fig_height = 3,
                          dev = 'png',
                          css = NULL,
                          ...) {

  if (is.null(css)) {
    css <- system.file("rmarkdown", "templates", "html_vignette" ,"resources",
      "vignette.css", package = "rmarkdown")
  }

  html_document(fig_width = fig_width,
                fig_height = fig_height,
                dev = dev,
                fig_retina = NULL,
                css = css,
                theme = NULL,
                highlight = "pygments",
                ...)
}
