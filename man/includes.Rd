\name{includes}
\alias{includes}
\title{Include content within output}
\usage{
includes(in.header = NULL, before.body = NULL, after.body = NULL)
}
\arguments{
  \item{in.header}{One or more files with content to be
  included in the header of the document.}

  \item{before.body}{One or more files with content to be
  included before the document body.}

  \item{after.body}{One or more files with content to be
  included after the document body.}
}
\value{
A list with content to be included in output.
}
\description{
Specify additional content to be included within an output
document.
}
\details{
Non-absolute paths for resources referenced from the
\code{in.header}, \code{before.body}, and \code{after.body}
parameters are resolved relative to the directory of the
input document.
}
\examples{
\dontrun{

library(rmarkdown)

html_document(includes = includes(before.body = "header.htm"))

pdf_document(includes = includes(after.body = "footer.tex"))

}
}

