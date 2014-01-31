\name{knitr_options_html}
\alias{knitr_options_html}
\title{Knitr options for an HTML output format}
\usage{
knitr_options_html(fig.width, fig.height, fig.retina)
}
\arguments{
  \item{fig.width}{Default width (in inches) for figures}

  \item{fig.height}{Default width (in inches) for figures}

  \item{fig.retina}{Scaling to perform for retina displays
  (defaults to 2, which currently works for all widely used
  retina displays). Note that this only takes effect if you
  are using knitr >= 1.5.21. Set to \code{NULL} to prevent
  retina scaling.}
}
\value{
An list that can be passed as the \code{knitr} argument of
the \code{\link{output_format}} function.
}
\description{
Define knitr options for an R Markdown output format that
creates HTML output.
}
\seealso{
\link{knitr_options}, \link{output_format}
}

