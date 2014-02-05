\name{includes}
\alias{includes}
\title{Include content within output}
\usage{
includes(in_header = NULL, before_body = NULL, after_body = NULL)
}
\arguments{
  \item{in_header}{One or more files with content to be
  included in the header of the document.}

  \item{before_body}{One or more files with content to be
  included before the document body.}

  \item{after_body}{One or more files with content to be
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
\code{in_header}, \code{before_body}, and \code{after_body}
parameters are resolved relative to the directory of the
input document.
}
\examples{
\dontrun{

library(rmarkdown)

html_document(includes = includes(before_body = "header.htm"))

pdf_document(includes = includes(after_body = "footer.tex"))

}
}

