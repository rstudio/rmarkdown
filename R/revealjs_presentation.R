#' Convert to a reveal.js presentation
#'
#' Format for converting from R Markdown to a reveal.js presentation.
#'
#' @inheritParams beamer_presentation
#' @inheritParams html_document
#'
#' @param theme
#' @param transition
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' In reveal.js presentations, if slide_level is 2, a two-dimensional layout
#' will be produced, with level 1 headers building horizontally and level 2
#' headers building vertically. It is not recommended that you use deeper
#' nesting of section levels with reveal.js.
#'
#' For more information on markdown syntax for presentations see
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
#' slide shows with pandoc}.
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' # simple invocation
#' render("pres.Rmd", revealjs_presentation())
#'
#' # specify an option for incremental rendering
#' render("pres.Rmd", revealjs_presentation(incremental = TRUE))
#' }
#'
#' @export
revealjs_presentation <- function(slide_level = NULL,
                                  incremental = FALSE,
                                  fig_width = 8,
                                  fig_height = 5,
                                  fig_retina = 2,
                                  fig_caption = FALSE,
                                  highlight = "default",
                                  theme = "default",
                                  transition = "default",
                                  mathjax = "default",
                                  template = NULL,
                                  includes = NULL,
                                  data_dir = NULL,
                                  pandoc_args = NULL) {

  # base pandoc options for all reveal.js output
  args <- c()

  # template path and assets
  if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))
  else {
    args <- c(args, "--template",
              pandoc_path_arg(rmarkdown_system_file(
                                  "rmd/revealjs/default.html")))
  }

  # slide level
  if (!is.null(slide_level))
    args <- c(args, "--slide-level", as.character(slide_level))

  # incremental
  if (incremental)
    args <- c(args, "--incremental")

  # theme
  theme <- match.arg(theme, revealjs_themes())
  if (!identical(theme, "default"))
    args <- c(args, "--variable", paste("theme=", theme, sep=""))

  # transition
  transition <- match.arg(transition, revealjs_transitions())
  if (!identical(transition, "default"))
    args <- c(args, "--variable", paste("transition=", transition, sep=""))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # data dir
  if (!is.null(data_dir))
    args <- c(args, "--data-dir", pandoc_path_arg(data_dir))

  # custom args
  args <- c(args, pandoc_args)

  # build a filter we'll use for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  filter <- function(output_format, files_dir, input_lines) {

    # extra args
    args <- c()

    # reveal.js
    revealjs_path <- rmarkdown_system_file("rmd/revealjs/reveal.js-2.6.1")
    revealjs <- render_supporting_files(revealjs_path, files_dir)
    args <- c(args, "--variable", paste("revealjs-url=", revealjs, sep=""))

    # highlight
    args <- c(args, pandoc_html_highlight_args(highlight,
                                               template,
                                               FALSE,
                                               files_dir))

    # mathjax
    if (!is.null(mathjax))
      args <- c(args, pandoc_mathjax_args(mathjax, files_dir))

    # return format with ammended args
    output_format$pandoc$args <- c(output_format$pandoc$args, args)
    output_format
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina),
    pandoc = pandoc_options(to = "revealjs",
                            from_rmarkdown(fig_caption),
                            args = args),
    clean_supporting = FALSE,
    filter = filter
  )
}

revealjs_themes <- function() {
  c("default",
    "sky",
    "beige",
    "simple",
    "serif",
    "night",
    "moon",
    "solarized")
}


revealjs_transitions <- function() {
  c("default",
    "cube",
    "page",
    "concave",
    "zoom",
    "linear",
    "fade",
    "none")
}


