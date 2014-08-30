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
#' @inheritParams html_document
#' @return R Markdown output format to pass to \code{\link{render}}
#' @export
html_vignette <- function(fig_width = 3,
                          fig_height = 3,
                          css = NULL,
                          ...) {
  
  if (is.null(css)) {
    css <- system.file("rmarkdown", "templates", "html_vignette" ,"resources", 
      "vignette.css", package = "rmarkdown")
  }
  
  html_document(fig_width = fig_width, 
                fig_height = fig_height, 
                fig_retina = FALSE,
                css = css, 
                theme = NULL,
                highlight = "pygments",
                ...)
}
