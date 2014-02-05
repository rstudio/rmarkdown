\name{render}
\alias{render}
\title{Render R Markdown}
\usage{
render(input, output.format = NULL, output.file = NULL, clean = TRUE,
  envir = parent.frame(), quiet = FALSE, encoding = getOption("encoding"))
}
\arguments{
  \item{input}{Input file (Rmd or plain markdown)}

  \item{output.format}{R Markdown output format to convert to. This can
  be a character vector with the name of a format; e.g. \code{"html_document"}.
  Alternatively it can be an output format object; e.g. \code{html_document()}.
  If \code{NULL} is passed then the output format is read from the YAML
  metadata within the input file (defaulting to HTML if no format is specified
  in metadata).}

  \item{output.file}{Output file (if not specified then a
  default based on the specified output format is chosen)}

  \item{clean}{\code{TRUE} to clean intermediate files created
  during rendering.}

  \item{envir}{The environment in which the code chunks are
  to be evaluated during knitting (can use
  \code{\link{new.env}()} to guarantee an empty new
  environment)}

  \item{quiet}{\code{TRUE} to supress printing of the
  pandoc command line}

  \item{encoding}{the encoding of the input file; see
  \code{\link{file}}}
}
\value{
The compiled document is written into the output file, and
the path of the output file is returned.
}
\description{
Render the input file to the specified output format using
pandoc. If the input requires knitting then
\code{\link[knitr:knit]{knit}} is called prior to pandoc.
}
\details{
If no \code{output} parmaeter is specified then the output
format is read from the YAML front-matter of the input
file. For example, the following YAML would yield a PDF
document:

\preformatted{
output: pdf_document
}

Additional format options can also be specified in metadata. For example:

\preformatted{
output:
  pdf_document:
    toc: true
    highlight: zenburn
}

Multiple formats can be specified in metadata. If no \code{output.format}
is passed to \code{render} then the first one defined will be used:

\preformatted{
output:
  pdf_document:
    toc: true
    highlight: zenburn
  html_document:
    toc: true
    theme: united
}

Formats specified in metadata can be any one of the built in formats
(e.g. \code{\link{html_document}},
\code{\link{pdf_document}}) or a format defined in another
package (e.g. \code{pkg::custom_format}).

If there is no format defined in the YAML then
\code{\link{html_document}} will be used.
}
\section{R Markdown}{
  R Markdown supports all of the base pandoc markdown
  features as well as some optional features for
  compatibility with GitHub Flavored Markdown (which
  previous versions of R Markdown were based on).

  For more on pandoc markdown see the
  \href{http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html}{pandoc
  markdown specification}. Optional pandoc markdown
  features enabled include \code{autolink_bare_uris},
  \code{ascii_identifiers}, and
  \code{tex_math_single_backslash}.
}
\seealso{
\link[knitr:knit]{knit}, \link{output_format},
\href{http://johnmacfarlane.net/pandoc}{pandoc}
}

