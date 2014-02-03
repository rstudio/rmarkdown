\name{pandoc_convert}
\alias{pandoc_convert}
\title{Convert a document with pandoc}
\usage{
pandoc_convert(input, to, from = NULL, output = NULL, citeproc = FALSE,
  options = NULL, verbose = FALSE)
}
\arguments{
  \item{input}{Input file (must be UTF-8 encoded)}

  \item{to}{Format to convert to}

  \item{from}{Format to convert from (if not specified then
  the format is determined based on the file extension of
  \code{input}).}

  \item{output}{Output file (if not specified then
  determined based on format being converted to)}

  \item{citeproc}{\code{TRUE} to run the pandoc-citeproc
  filter (for processing citations) as part of the
  conversion}

  \item{options}{Character vector of command line options
  to pass to pandoc.}

  \item{verbose}{\code{TRUE} to show the pandoc command
  line which was executed}
}
\description{
Convert documents to and from various formats using the
pandoc utility.
}
\details{
Supported input and output formats are described in the
\href{http://johnmacfarlane.net/pandoc/README.html}{pandoc
user guide}.

The system path as well as the version of pandoc shipped
with RStudio (if running under RStudio) are scanned for
pandoc and the highest version available is used.
}
\examples{
\dontrun{
library(rmarkdown)

# convert markdown to various formats
pandoc_convert("input.md", to = "html")
pandoc_convert("input.md", to = "pdf")

# process citations
pandoc_convert("input.md", to = "html", citeproc = TRUE)

# add some pandoc options
pandoc_convert("input.md", to="pdf, options = c("--listings"))
}
}

