\name{revealjs_presentation}
\alias{revealjs_presentation}
\title{Convert to a reveal.js presentation}
\usage{
revealjs_presentation(slide_level = NULL, incremental = FALSE,
  center = FALSE, fig_width = 8, fig_height = 6, fig_retina = 2,
  fig_caption = FALSE, theme = "default", transition = "default",
  highlight = "default", mathjax = "default", template = NULL,
  includes = NULL, data_dir = NULL, pandoc_args = NULL)
}
\arguments{
  \item{center}{\code{TRUE} to vertically center content on
  slides}

  \item{theme}{Visual theme ("default", "sky", "beige",
  "simple", "serif", or "solarized").}

  \item{transition}{Slide transition ("default", "cube",
  "page", "concave", "zoom", "linear", "fade", or "none")}

  \item{slide_level}{The heading level which defines
  indvidual slides. By default this is the highest header
  level in the hierarchy that is followed immediately by
  content, and not another header, somewhere in the
  document.  This default can be overridden by specifying
  an explicit \code{slide.level}.}

  \item{incremental}{\code{TRUE} to render slide bullets
  incrementally. Note that if you want to reverse the
  default incremental behavior for an individual bullet you
  can preceded it with \code{>}. For example: \emph{\code{>
  - Bullet Text}}}

  \item{fig_width}{Default width (in inches) for figures}

  \item{fig_height}{Default width (in inches) for figures}

  \item{fig_caption}{\code{TRUE} to render figures with
  captions}

  \item{highlight}{Syntax highlighting style. Supported
  styles include "default", "tango", "pygments", "kate",
  "monochrome", "espresso", "zenburn", and "haddock". Pass
  \code{NULL} to prevent syntax highlighting.}

  \item{template}{Pandoc template to use for rendering
  content. See the documentation on
  \href{http://johnmacfarlane.net/pandoc/demo/example9/templates.html}{pandoc
  templates} for more details.}

  \item{includes}{Named list of additional content to
  include within the document (typically created using the
  \code{\link{includes}} function).}

  \item{data_dir}{Additional directory to resolve relatives
  paths of templates and included content against (the
  directory of the input file is used by default).}

  \item{pandoc_args}{Additional command line options to
  pass to pandoc}

  \item{fig_retina}{Scaling to perform for retina displays
  (defaults to 2, which currently works for all widely used
  retina displays). Note that this only takes effect if you
  are using knitr >= 1.5.21. Set to \code{NULL} to prevent
  retina scaling.}

  \item{mathjax}{Include mathjax. The "default" option uses
  an https URL from the official MathJax CDN. The "local"
  option uses a local version of MathJax (which is copied
  into the output directory). You can pass an alternate URL
  or pass \code{NULL} to exclude MathJax entirely.}
}
\value{
R Markdown output format to pass to \code{\link{render}}
}
\description{
Format for converting from R Markdown to a reveal.js
presentation.
}
\details{
In reveal.js presentations, if slide_level is 2, a
two-dimensional layout will be produced, with level 1
headers building horizontally and level 2 headers building
vertically. It is not recommended that you use deeper
nesting of section levels with reveal.js.

For more information on markdown syntax for presentations
see
\href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
slide shows with pandoc}.

Unlike web content produced with
\code{\link{html_document}}, reveal.js presentations are
not standalone web pages. Rather, they have a set of
additional files they depend on (including the reveal.js
library) which are written into a files directory alongside
the presentation HTML (e.g. "MyPresentation_files").
}
\examples{
\dontrun{

library(rmarkdown)

# simple invocation
render("pres.Rmd", revealjs_presentation())

# specify an option for incremental rendering
render("pres.Rmd", revealjs_presentation(incremental = TRUE))
}
}

