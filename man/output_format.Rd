\name{output_format}
\alias{output_format}
\title{Define an R Markdown output format}
\usage{
output_format(to, knitr, pandoc, filter = NULL)
}
\arguments{
  \item{to}{Pandoc format to convert to}

  \item{knitr}{List of knitr options and hooks. Valid list
  elements include
  \code{\link[knitr:opts_knit]{opts_knit}},
  \code{\link[knitr:opts_chunk]{opts_chunk}}, and
  \code{\link[knitr:knit_hooks]{knit_hooks}}.}

  \item{pandoc}{Character vector of pandoc command line
  arguments}

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
\seealso{
\link{render}
}

