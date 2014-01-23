\name{pandocIncludeOptions}
\alias{pandocIncludeOptions}
\title{Pandoc options for including additional content}
\usage{
pandocIncludeOptions(include.header = NULL, include.before = NULL,
  include.after = NULL)
}
\arguments{
  \item{include.header}{One or more files with content to
  be included in the header of the document.}

  \item{include.before}{One or more files with content to
  be included before the document body.}

  \item{include.after}{One or more files with content to be
  included after the document body.}
}
\value{
A character vector with pandoc command line arguments
}
\description{
Options for including HTML or LaTeX content in the header
or before and after the document body.
}
\details{
Non-absolute paths for resources referenced from the
\code{include.header}, \code{include.before}, and
\code{include.after} parameters are resolved relative to
the directory of the input document.
}

