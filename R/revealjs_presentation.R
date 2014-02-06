#' Convert to a reveal.js presentation
#'
#' Format for converting from R Markdown to a reveal.js presentation.
#'
#' @inheritParams html_document
#' @inheritParams beamer_presentation
#'
#' @param center \code{TRUE} to vertically center content on slides
#' @param theme Visual theme ("default", "simple", sky", "beige", "serif", or
#'   "solarized").
#' @param transition Slide transition ("default", "cube", "page", "concave",
#'   "zoom", "linear", "fade", or "none")
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
#' Unlike web content produced with \code{\link{html_document}}, reveal.js
#' presentations are not standalone web pages. Rather, they have a set of
#' additional files they depend on (including the reveal.js library) which are
#' written into a files directory alongside the presentation HTML (e.g.
#' "MyPresentation_files").
#'
#' @section Templates:
#'
#' You can provide a custom HTML template to be used for rendering. The syntax
#' for templates is described in the documentation on
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/templates.html}{pandoc
#' templates}.
#'
#' Note however that if you choose to provide your own HTML template then
#' several aspects of HTML document rendering will behave differently:
#'
#' \itemize{
#'   \item{The \code{center} parameter does not work (you'd need to
#'      set this directly in the template).
#'   }
#'   \item{For the \code{highlight} parameter, the default highlighting
#'      style will resolve to "pygments" and the "textmate" highlighting
#'      style is not available.
#'   }
#'   \item{The built-in template includes some additional tweaks to styles
#'      to optimize for output from R, these won't be present.
#'   }
#'   \item{MathJax is automatically disabled if \code{self_contained} is
#'      \code{TRUE} (these two options can't be used together in normal
#'      pandoc templates).
#'   }
#' }
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
                                  center = FALSE,
                                  fig_width = 8,
                                  fig_height = 6,
                                  fig_retina = 2,
                                  fig_caption = FALSE,
                                  smart = TRUE,
                                  self_contained = TRUE,
                                  theme = "default",
                                  transition = "default",
                                  highlight = "default",
                                  mathjax = "default",
                                  template = NULL,
                                  includes = NULL,
                                  data_dir = NULL,
                                  pandoc_args = NULL) {


  # interplay between arguments
  self_contained <- reconcile_self_contained(self_contained, mathjax)

  # base pandoc options for all reveal.js output
  args <- c()

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  # self contained document
  if (self_contained)
    args <- c(args, "--self-contained")

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

  # centering
  if (center)
    args <- c(args, "--variable", "center")

  # theme
  theme <- match.arg(theme, revealjs_themes())
  if (identical(theme, "default"))
    theme <- "simple"
  args <- c(args, "--variable", paste("theme=", theme, sep=""))


  # transition
  transition <- match.arg(transition, revealjs_transitions())
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
    if (!self_contained)
      revealjs_path <- render_supporting_files(revealjs_path, files_dir)
    args <- c(args, "--variable", paste("revealjs-url=", revealjs_path, sep=""))

    # highlight
    args <- c(args, pandoc_html_highlight_args(highlight,
                                               template,
                                               self_contained,
                                               files_dir))

    # mathjax
    args <- c(args, pandoc_mathjax_args(mathjax,
                                        template,
                                        self_contained,
                                        files_dir))

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
    clean_supporting = self_contained,
    filter = filter
  )
}

revealjs_themes <- function() {
  c("default",
    "simple",
    "sky",
    "beige",
    "serif",
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


