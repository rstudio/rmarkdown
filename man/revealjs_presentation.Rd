\name{revealjs_presentation}
\alias{revealjs_presentation}
\title{Convert to a reveal.js presentation}
\usage{
revealjs_presentation(incremental = FALSE, center = FALSE, fig_width = 8,
  fig_height = 6, fig_retina = 2, fig_caption = FALSE, smart = TRUE,
  self_contained = TRUE, theme = "default", transition = "default",
  highlight = "default", mathjax = "default", template = "default",
  includes = NULL, lib_dir = NULL, data_dir = NULL, pandoc_args = NULL,
  ...)
}
\arguments{
  \item{center}{\code{TRUE} to vertically center content on
  slides}

  \item{theme}{Visual theme ("default", "simple", sky",
  "beige", "serif", or "solarized").}

  \item{transition}{Slide transition ("default", "cube",
  "page", "concave", "zoom", "linear", "fade", or "none")}

  \item{template}{Pandoc template to use for rendering.
  Pass "default" to use the rmarkdown package default
  template; pass \code{NULL} to use pandoc's built-in
  template; pass a path to use a custom template that
  you've created. Note that if you don't use the "default"
  template then some features of
  \code{revealjs_presentation} won't be available (see the
  Templates section below for more details).}

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

  \item{smart}{Produce typographically correct output,
  converting straight quotes to curly quotes, --- to
  em-dashes, -- to en-dashes, and ... to ellipses.}

  \item{self_contained}{Produce a standalone HTML file with
  no external dependencies, using data: URIs to incorporate
  the contents of linked scripts, stylesheets, images, and
  videos. Note that even for self contained documents
  MathJax is still loaded externally (this is necessary
  because of it's size).}

  \item{mathjax}{Include mathjax. The "default" option uses
  an https URL from the official MathJax CDN. The "local"
  option uses a local version of MathJax (which is copied
  into the output directory). You can pass an alternate URL
  or pass \code{NULL} to exclude MathJax entirely.}

  \item{lib_dir}{Directory to copy dependent HTML libraries
  (e.g. jquery, bootstrap, etc.) into. By default this will
  be the name of the document with \code{_files} appended
  to it.}
}
\value{
R Markdown output format to pass to \code{\link{render}}
}
\description{
Format for converting from R Markdown to a reveal.js
presentation.
}
\details{
In reveal.js presentations you can use level 1 or level 2
headers for slides. If you use a mix of level 1 and level 2
headers then a two-dimensional layout will be produced,
with level 1 headers building horizontally and level 2
headers building vertically.

For more information on markdown syntax for presentations
see
\href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
slide shows with pandoc}.
}
\section{Templates}{
  You can provide a custom HTML template to be used for
  rendering. The syntax for templates is described in the
  documentation on
  \href{http://johnmacfarlane.net/pandoc/demo/example9/templates.html}{pandoc
  templates}. You can also use the basic pandoc template by
  passing \code{template = NULL}.

  Note however that if you choose not to use the "default"
  reveal.js template then several aspects of reveal.js
  presentation rendering will behave differently:

  \itemize{ \item{The \code{center} parameter does not work
  (you'd need to set this directly in the template).  }
  \item{The built-in template includes some additional
  tweaks to styles to optimize for output from R, these
  won't be present.  } \item{MathJax will not work if
  \code{self_contained} is \code{TRUE} (these two options
  can't be used together in normal pandoc templates).  } }
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

