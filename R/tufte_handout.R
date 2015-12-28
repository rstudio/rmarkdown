
#' Tufte handout format (PDF)
#' 
#' Template for creating a handout according to the style of 
#' Edward R. Tufte and Richard Feynman. 
#' 
#' @inheritParams pdf_document
#' 
#' @export
tufte_handout <- function(fig_width = 4,
                          fig_height = 2.5,
                          fig_crop = TRUE,
                          dev = 'pdf',
                          highlight = "default",
                          keep_tex = FALSE,
                          citation_package = c("none", "natbib", "biblatex"),
                          includes = NULL,
                          md_extensions = NULL,
                          pandoc_args = NULL) {
  
  # resolve default highlight
  if (identical(highlight, "default"))
    highlight <- "pygments"
  
  # get the tufte handlout template
  template <-  system.file(
    "rmarkdown/templates/tufte_handout/resources/tufte-handout.tex", 
    package = "rmarkdown"
  )
  
  # call the base pdf_document format with the appropriate options
  format <- rmarkdown::pdf_document(fig_width = fig_width,
                                    fig_height = fig_height,
                                    fig_crop = fig_crop,
                                    dev = dev,
                                    highlight = highlight,
                                    template = template,
                                    keep_tex = keep_tex,
                                    citation_package = citation_package,
                                    latex_engine = "pdflatex",
                                    includes = includes,
                                    md_extensions = md_extensions,
                                    pandoc_args = pandoc_args)
                        
  
  # create knitr options (ensure opts and hooks are non-null)
  knitr_options <- knitr_options_pdf(fig_width, fig_height, fig_crop, dev)
  if (is.null(knitr_options$opts_knit))
    knitr_options$opts_knit <- list()
  if (is.null(knitr_options$knit_hooks))
    knitr_options$knit_hooks <- list()
  
  # set options
  knitr_options$opts_chunk$tidy <- TRUE
  knitr_options$opts_knit$width <- 45
  
  # set hooks for special plot output
  knitr_options$knit_hooks$plot <- function(x, options) {

    # determine figure type
    if (isTRUE(options$fig.margin)) 
      options$fig.env <- "marginfigure"
    else if (isTRUE(options$fig.fullwidth))
      options$fig.env <- "figure*"

    knitr::hook_plot_tex(x, options)
  }
  
  # override the knitr settings of the base format and return the format
  format$knitr <- knitr_options
  format
}

#' Tufte handout format (HTML)
#' 
#' Similar to \code{\link{tufte_handout}}, but for HTML output. It uses the CSS 
#' from \url{https://edwardtufte.github.io/tufte-css/}.
#' @param css One or more CSS files to include, and the Tufte CSS will always be
#'   included in this list
#' @param ... Other arguments to be passed to \code{\link{html_document}} (note
#'   you cannot use the \code{theme} argument, which has been set to \code{NULL}
#'   internally)
#' @export
tufte_html <- function(css = NULL, ...) {
  tufte_css <- rmarkdown_system_file(
    'rmarkdown', 'templates', 'tufte_html', 'resources', 'tufte.css'
  )
  css <- c(tufte_css, css)
  html_document(css = css, theme = NULL, ...)
}
