\name{rmarkdown_format}
\alias{rmarkdown_format}
\title{R Markdown input format definition}
\usage{
rmarkdown_format(extensions = NULL)
}
\arguments{
  \item{extensions}{Markdown extensions to be added or
  removed from the default definition of R Markdown.}
}
\value{
Pandoc markdown format specification
}
\description{
Compose a pandoc markdown input definition for R Markdown
that can be passed as the \code{from} argument of
\link{pandoc_options}.
}
\details{
By default R Markdown is defined as all pandoc markdown
extensions plus \code{autolink_bare_uris},
\code{ascii_identifier}, and
\code{tex_math_single_backslash}.

For more on pandoc markdown see the
\href{http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html}{pandoc
markdown specification}.
}
\examples{
\dontrun{
rmarkdown_format("-implicit_figures")
}
}
\seealso{
\link{output_format}, \link{pandoc_options}
}

