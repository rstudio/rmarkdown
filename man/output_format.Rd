\name{output_format}
\alias{output_format}
\title{Define an R Markdown output format}
\usage{
output_format(knitr, pandoc, filter = NULL)
}
\arguments{
  \item{knitr}{Knitr options for an output format (see
  \code{\link{knitr_options}})}

  \item{pandoc}{Pandoc options for an output format (see
  \code{\link{pandoc_options}})}

  \item{filter}{An optional filter function that receieves
  the format and lines of the input file as input and can
  return a modified format.}
}
\value{
An R Markdown output format definition that can be passed
to \code{\link{render}}.
}
\description{
Define an R Markdown output format based on a combination
of knitr and pandoc options.
}
\examples{
\dontrun{
output_format(knitr = knitr_options(opts_chunk = list(dev = 'png')),
              pandoc = pandoc_options(to = "html"))
}
}
\seealso{
\link{render}, \link{knitr_options}, \link{pandoc_options}
}

