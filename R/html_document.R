#' Convert to an HTML document
#'
#' Format for converting from R Markdown to an HTML document.
#'
#' @param toc \code{TRUE} to include a table of contents in the output
#' @param toc.depth Depth of headers to include in table of contents
#' @param fig.width Default width (in inches) for figures
#' @param fig.height Default width (in inches) for figures
#' @param fig.retina Scaling to perform for retina displays (defaults to 2,
#'   which currently works for all widely used retina displays). Note that this
#'   only takes effect if you are using knitr >= 1.5.21. Set to \code{NULL} to
#'   prevent retina scaling.
#' @param fig.caption \code{TRUE} to render figures with captions
#' @param smart Produce typographically correct output, converting straight
#'   quotes to curly quotes, --- to em-dashes, -- to en-dashes, and ... to
#'   ellipses.
#' @param self.contained Produce a standalone HTML file with no external
#'   dependencies, using data: URIs to incorporate the contents of linked
#'   scripts, stylesheets, images, and videos.
#' @param theme Visual theme ("default", "cerulean", "journal", "flatly",
#'   "readable", "spacelab", "united", "yeti", or "cosmo"). Pass \code{NULL}
#'    for no theme (in this case you can use the \code{css} parameter to
#'    add your own styles).
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "pygments", "kate", "monochrome", "espresso", "zenburn",
#'   "haddock", "tango", and "textmate". Pass \code{NULL} to prevent syntax
#'   highlighting.
#' @param mathjax Include mathjax. The "default" option uses an https URL from
#'   the official MathJax CDN. The "local" option uses a local version of
#'   MathJax (which is copied into the output directory). You can pass an
#'   alternate URL or pass \code{NULL} to exclude MathJax entirely.
#' @param css One or more css files to include
#' @param includes Named list of additional content to include within the
#'   document (typically created using the \code{\link{includes}} function).
#' @param pandoc.args Additional command line options to pass to pandoc
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
#' render("input.Rmd", html_document())
#'
#' render("input.Rmd", html_document(toc = TRUE))
#' }
#'
#' @export
html_document <- function(toc = FALSE,
                          toc.depth = 3,
                          fig.width = 7,
                          fig.height = 5,
                          fig.retina = 2,
                          fig.caption = FALSE,
                          smart = TRUE,
                          self.contained = TRUE,
                          theme = "default",
                          highlight = "default",
                          mathjax = "default",
                          css = NULL,
                          includes = NULL,
                          pandoc.args = NULL) {

  # build pandoc args
  args <- c("--standalone")

  # smart quotes, etc.
  if (smart)
    args <- c(args, "--smart")

  # self contained document
  if (self.contained)
    args <- c(args, "--self-contained")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc.depth))

  # template path and assets
  args <- c(args, "--template", pandoc_template("h/default.html"))

  # theme
  if (!is.null(theme)) {
    theme <- match.arg(theme, themes())
    if (identical(theme, "default"))
      theme <- "bootstrap"
    theme <- paste("bootstrap/css/", theme, ".min.css", sep="")
    theme <- pandoc_template(file.path("h", theme))
    args <- c(args, "--variable", paste("theme:", theme, sep=""))
  }

  # highlighting
  if (is.null(highlight)) {
    args <- c(args, "--no-highlight")
  } else {
    highlight <- match.arg(highlight, html_highlighters())
    if (highlight %in% c("default", "textmate")) {
      args <- c(args, "--no-highlight")
      highlight_path <- pandoc_template("h/highlight")
      args <- c(args,
                "--variable", paste("highlightjs=", highlight_path, sep=""))
      if (identical(highlight, "textmate")) {
        args <- c(args,
                  "--variable", paste("highlightjs-theme=", highlight, sep=""))
      }
    }
    else {
      args <- c(args, "--highlight-style", highlight)
    }
  }

  # mathjax (track whether we specified local mathjax)
  local_mathjax <- FALSE
  if (!is.null(mathjax)) {
    if (identical(mathjax, "default")) {
      mathjax <- default_mathjax()
    }
    else if (identical(mathjax, "local")) {
      local_mathjax <- TRUE
      mathjax <- local_mathjax()
    }
    args <- c(args, "--mathjax")
    args <- c(args, "--variable", paste("mathjax-url:", mathjax, sep=""))
  }

  # additional css
  for (css_file in css)
    args <- c(args, "--css", css_file)

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # pandoc args
  args <- c(args, pandoc.args)

  # return format
  output_format(
    knitr = knitr_options_html(fig.width, fig.height, fig.retina),
    pandoc = pandoc_options(to = "html",
                            from = from_rmarkdown(fig.caption),
                            args = args),
    filter <- filter_html(local_mathjax)
  )
}


#' Knitr options for an HTML output format
#'
#' Define knitr options for an R Markdown output format that creates
#' HTML output.
#'
#' @inheritParams html_document
#'
#' @return An list that can be passed as the \code{knitr} argument of the
#'   \code{\link{output_format}} function.
#'
#' @seealso \link{knitr_options}, \link{output_format}
#'
#' @export
knitr_options_html <- function(fig.width, fig.height, fig.retina) {
  knitr_options(
    opts_chunk = list(dev = 'png',
                      dpi = 96,
                      fig.width = fig.width,
                      fig.height = fig.height,
                      fig.retina = fig.retina)
  )
}

# Filter to copy mathjax files to the output directory if necessary
filter_html <- function(local_mathjax) {

  function(output.format, output.file, input) {

    if (local_mathjax) {
      output_dir <- dirname(output.file)
      mathjax_stage_dir <- file.path(output_dir, "m")
      mathjax_dir <- file.path(output_dir, "mathjax")
      if (!file.exists(mathjax_dir) && !file.exists(mathjax_stage_dir)) {
        dir.create(mathjax_dir)
        file.copy(from = file.path(pandoc_template("h"), "m"),
                  to = output_dir,
                  recursive = TRUE)
        file.rename(from = file.path(output_dir, "m"),
                    to = mathjax_dir)
      }
    }

    output.format
  }

}

themes <- function() {
  c("default",
    "cerulean",
    "journal",
    "flatly",
    "readable",
    "spacelab",
    "united",
    "yeti",
    "cosmo")
}

html_highlighters <- function() {
  c(highlighters(), "textmate")
}

default_mathjax <- function() {
  paste("https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/",
        mathjax_config(), sep="")
}

local_mathjax <- function() {
  paste("mathjax/", mathjax_config(), sep="")
}

mathjax_config <- function() {
  "MathJax.js?config=TeX-AMS-MML_HTMLorMML"
}


