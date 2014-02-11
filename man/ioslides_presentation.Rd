\name{ioslides_presentation}
\alias{ioslides_presentation}
\title{Convert to an ioslides Presentation}
\usage{
ioslides_presentation(fig_width = 7.5, fig_height = 5.5, fig_retina = 2,
  fig_caption = FALSE, smart = TRUE, self_contained = TRUE,
  widescreen = FALSE, smaller = FALSE, mathjax = "default", css = NULL,
  includes = NULL, data_dir = NULL, pandoc_args = NULL)
}
\arguments{
  \item{includes}{Named list of additional content to
  include within the document (typically created using the
  \code{\link{includes}} function).  Note that if
  \code{before_body} content is specified it will replace
  the standard title slide entirely.}
}
\description{
Convert to an ioslides Presentation
}

