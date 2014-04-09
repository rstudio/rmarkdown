#' Convert to a reveal.js presentation
#'
#' Format for converting from R Markdown to a reveal.js presentation.
#'
#' @inheritParams beamer_presentation
#' @inheritParams pdf_document
#' @inheritParams html_document
#'
#' @param center \code{TRUE} to vertically center content on slides
#' @param theme Visual theme ("default", "simple", sky", "beige", "serif", or
#'   "solarized").
#' @param transition Slide transition ("default", "cube", "page", "concave",
#'   "zoom", "linear", "fade", or "none")
#' @param template Pandoc template to use for rendering. Pass "default"
#'   to use the rmarkdown package default template; pass \code{NULL}
#'   to use pandoc's built-in template; pass a path to use a custom template
#'   that you've created. Note that if you don't use the "default" template
#'   then some features of \code{revealjs_presentation} won't be available
#'   (see the Templates section below for more details).
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' In reveal.js presentations you can use level 1 or level 2 headers for
#' slides. If you use a mix of level 1 and level 2 headers then a
#' two-dimensional layout will be produced, with level 1 headers building
#' horizontally and level 2 headers building vertically.
#'
#' For more information on markdown syntax for presentations see
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
#' slide shows with pandoc}.
#'
#' @section Templates:
#'
#' You can provide a custom HTML template to be used for rendering. The syntax
#' for templates is described in the documentation on
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/templates.html}{pandoc
#' templates}. You can also use the basic pandoc template by passing
#' \code{template = NULL}.
#'
#' Note however that if you choose not to use the "default" reveal.js template
#' then several aspects of reveal.js presentation rendering will behave
#' differently:
#'
#' \itemize{
#'   \item{The \code{center} parameter does not work (you'd need to
#'      set this directly in the template).
#'   }
#'   \item{The built-in template includes some additional tweaks to styles
#'      to optimize for output from R, these won't be present.
#'   }
#'   \item{MathJax will not work if \code{self_contained} is \code{TRUE}
#'      (these two options can't be used together in normal pandoc templates).
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
revealjs_presentation <- function(incremental = FALSE,
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
                                  template = "default",
                                  includes = NULL,
                                  lib_dir = NULL,
                                  data_dir = NULL,
                                  pandoc_args = NULL) {

  # base pandoc options for all reveal.js output
  args <- c()

  # no email obfuscation
  args <- c(args, "--email-obfuscation", "none")

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  # self contained document
  if (self_contained) {
    validate_self_contained(mathjax)
    args <- c(args, "--self-contained")
  }

  # template path and assets
  if (identical(template, "default"))
    args <- c(args, "--template",
              pandoc_path_arg(rmarkdown_system_file(
                "rmd/revealjs/default.html")))
  else if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))

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
  else if (identical(theme, "dark"))
    theme <- "default"
  if (theme %in% c("default", "blood", "moon", "night"))
    args <- c(args, "--variable", "theme-dark")
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

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(input_lines, runtime, knit_meta, files_dir) {

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # extra args
    args <- c()

    # resolve and inject extras
    extras <- html_extras_for_document(knit_meta, runtime)
    args <- c(args, pandoc_html_extras_args(extras, self_contained, lib_dir))

    # reveal.js
    revealjs_path <- rmarkdown_system_file("rmd/revealjs/reveal.js-2.6.1")
    if (!self_contained)
      revealjs_path <- render_supporting_files(revealjs_path, lib_dir)
    args <- c(args, "--variable", paste("revealjs-url=",
                                        pandoc_path_arg(revealjs_path), sep=""))

    # highlight
    args <- c(args, pandoc_highlight_args(highlight, default = "pygments"))

    # mathjax
    args <- c(args, pandoc_mathjax_args(mathjax,
                                        template,
                                        self_contained,
                                        lib_dir))

    # return additional args
    args
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina),
    pandoc = pandoc_options(to = "revealjs",
                            from = from_rmarkdown(fig_caption),
                            args = args),
    clean_supporting = self_contained,
    pre_processor = pre_processor
  )
}

revealjs_themes <- function() {
  c("default",
    "simple",
    "sky",
    "beige",
    "serif",
    "solarized",
    "dark",
    "blood",
    "moon",
    "night")
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


