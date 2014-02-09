\name{output_format}
\alias{output_format}
\title{Define an R Markdown output format}
\usage{
output_format(knitr, pandoc, clean_supporting = TRUE, format_filter = NULL)
}
\arguments{
  \item{knitr}{Knitr options for an output format (see
  \code{\link{knitr_options}})}

  \item{pandoc}{Pandoc options for an output format (see
  \code{\link{pandoc_options}})}

  \item{clean_supporting}{Cleanup any supporting files
  after conversion see
  \code{\link{render_supporting_files}}}

  \item{format_filter}{An optional filter function that
  receives the \code{output_format}, \code{files_dir}, and
  \code{input_lines}.  The filter can be called for it's
  side-effects (e.g. rendering supporting files to the
  \code{files_dir}) and can also return a modified format.}
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

