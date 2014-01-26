\name{rmd2docx}
\alias{rmd2docx}
\title{Convert R Markdown to MS Word}
\usage{
rmd2docx(input, options = docxOptions(), output = NULL,
  envir = parent.frame(), quiet = FALSE, encoding = getOption("encoding"))
}
\arguments{
  \item{input}{Input file (Rmd or plain markdown)}

  \item{options}{Character vector of pandoc options created
  by calling \code{\link{docxOptions}}}

  \item{output}{Target output file (defaults to
  <input>.docx if not specified)}

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
Converts the input file to MS Word docx using pandoc. If
the input requires knitting then
\code{\link[knitr:knit]{knit}} is called prior to pandoc.
}
\details{
R Markdown documents can have optional metadata that is
used to generate a document header that includes the title,
author, and date.  Metadata can also be provided to enable
the use of footnotes and bibliographies. For more details
see the documentation on \link[=rmdMetadata]{R Markdown
Metadata}.
}
\examples{
\dontrun{
library(rmarkdown)

# simple invocation
rmd2docx("input.Rmd")

# specify an option for syntax highlighting
rmd2docx("input.Rmd", docxOptions(highlight = "zenburn"))
}
}
\seealso{
\code{\link[knitr:knit]{knit}}, \code{\link{docxOptions}}
}

