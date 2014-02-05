\name{pandoc_args}
\alias{pandoc_args}
\alias{pandoc_highlight_args}
\alias{pandoc_include_args}
\alias{pandoc_toc_args}
\title{Functions for generating pandoc command line arguments}
\usage{
pandoc_include_args(in_header = NULL, before_body = NULL,
  after_body = NULL)

pandoc_highlight_args(highlight)

pandoc_toc_args(toc, toc_depth = 3)
}
\arguments{
  \item{toc}{\code{TRUE} to include a table of contents in
  the output}

  \item{toc_depth}{Depth of headers to include in table of
  contents}

  \item{highlight}{The name of a pandoc syntax highlighting
  theme.}

  \item{in_header}{One or more files with content to be
  included in the header of the document.}

  \item{before_body}{One or more files with content to be
  included before the document body.}

  \item{after_body}{One or more files with content to be
  included after the document body.}
}
\value{
A character vector with pandoc command line arguments
}
\description{
Functions that assist in creating various types of pandoc
command line arguments (e.g. for templates, table of
contents, highlighting, and content includes)
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

pandoc_include_args(before_body = "header.htm")
pandoc_include_args(before_body = "header.tex")

pancoc_highlight_args("kate")

pandoc_toc_args(toc = TRUE, toc_depth = 2)

}
}

