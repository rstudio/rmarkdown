\name{pandoc_options}
\alias{pandoc_options}
\title{Pandoc options for an output format}
\usage{
pandoc_options(to, from = rmarkdown_format(), args = NULL,
  keep_tex = FALSE)
}
\arguments{
  \item{to}{Pandoc format to convert to}

  \item{from}{Pandoc format to convert from}

  \item{args}{Character vector of command line arguments to
  pass to pandoc}

  \item{keep_tex}{Keep the intermediate tex file used in
  the conversion to PDF (applies only to 'latex' and
  'beamer' target formats)}
}
\value{
An list that can be passed as the \code{pandoc} argument of
the \code{\link{output_format}} function.
}
\description{
Define the pandoc options for an R Markdown output format.
}
\details{
The \code{from} argument should be used very cautiously as
it's important for users to be able to rely on a stable
definition of supported markdown extensions.
}
\seealso{
\link{output_format}, \link{rmarkdown_format}
}

