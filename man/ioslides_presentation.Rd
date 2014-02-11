\name{ioslides_presentation}
\alias{ioslides_presentation}
\title{Convert to an ioslides Presentation}
\usage{
ioslides_presentation(incremental = FALSE, fig_width = 7.5,
  fig_height = 5.5, fig_retina = 2, fig_caption = FALSE, smart = TRUE,
  self_contained = TRUE, widescreen = FALSE, smaller = FALSE,
  mathjax = "default", css = NULL, includes = NULL, data_dir = NULL,
  pandoc_args = NULL)
}
\arguments{
  \item{widescreen}{Display presentation with wider
  dimensions.}

  \item{smaller}{Use smaller text on all slides.}

  \item{incremental}{\code{TRUE} to render slide bullets
  incrementally. Note that if you want to reverse the
  default incremental behavior for an individual bullet you
  can preceded it with \code{>}. For example: \emph{\code{>
  - Bullet Text}}}

  \item{fig_width}{Default width (in inches) for figures}

  \item{fig_height}{Default width (in inches) for figures}

  \item{fig_caption}{\code{TRUE} to render figures with
  captions}

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
  videos. Note that if you specify "local" for
  \code{mathjax} then \code{self_contained} is
  automatically set to \code{FALSE}.}

  \item{mathjax}{Include mathjax. The "default" option uses
  an https URL from the official MathJax CDN. The "local"
  option uses a local version of MathJax (which is copied
  into the output directory). You can pass an alternate URL
  or pass \code{NULL} to exclude MathJax entirely.}

  \item{css}{One or more css files to include}
}
\value{
R Markdown output format to pass to \code{\link{render}}
}
\description{
Convert to an ioslides Presentation
}
\details{
If a \code{before_body} include is specified then it will
replace the standard title slide entirely.

For more information on markdown syntax for presentations
see
\href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
slide shows with pandoc}.
}

