\name{rmd2html}
\alias{rmd2html}
\title{Convert R Markdown to HTML}
\usage{
rmd2html(input, options = htmlOptions(), output = NULL,
  envir = parent.frame(), quiet = FALSE, encoding = getOption("encoding"))
}
\arguments{
  \item{input}{Input file (Rmd or plain markdown)}

  \item{options}{Character vector of pandoc options created
  by calling \code{\link{htmlOptions}}}

  \item{output}{Target output file (defaults to
  <input>.html if not specified)}

  \item{envir}{The environment in which the code chunks are
  to be evaluated during knitting (can use
  \code{\link{new.env}()} to guarantee an empty new
  environment)}

  \item{quiet}{Whether to suppress the progress bar and
  messages}

  \item{encoding}{The encoding of the input file; see
  \code{\link{file}}}
}
\value{
The compiled document is written into the output file, and
the path of the output file is returned.
}
\description{
Converts the input file to HTML using pandoc. If the input
requires knitting then \code{\link[knitr:knit]{knit}} is
called prior to pandoc.
}
\details{
R Markdown documents can have optional metadata that is
used to generate a document header that includes the title,
author, and date. Metadata can also be provided to enable
the use of footnotes and bibliographies. For more details
see the documentation on R Markdown
\link[=rmdMetadata]{metadata} and
\link[=rmdCitations]{citations}.
}
\examples{
\dontrun{

library(rmarkdown)

rmd2html("input.Rmd")

rmd2html("input.Rmd", htmlOptions(toc = TRUE))
}
}
\seealso{
\code{\link[knitr:knit]{knit}}, \code{\link{htmlOptions}}
}

