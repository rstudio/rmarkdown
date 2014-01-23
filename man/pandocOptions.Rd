\name{pandocOptions}
\alias{highlightOptions}
\alias{includeOptions}
\alias{pandocOptions}
\alias{tableOfContentsOptions}
\alias{templateOptions}
\title{Functions for Generating Pandoc Options}
\usage{
includeOptions(header = NULL, before.body = NULL, after.body = NULL)

templateOptions(template)

highlightOptions(highlight)

tableOfContentsOptions(toc, toc.depth)
}
\arguments{
  \item{toc}{\code{TRUE} to include a table of contents in
  the output}

  \item{toc.depth}{Depth of headers to include in table of
  contents}

  \item{template}{Path to a pandoc template to use for
  conversion (should be either absolute or realtive to the
  directory of the input file).}

  \item{highlight}{The name of a pandoc syntax highlighting
  theme.}

  \item{header}{One or more files with content to be
  included in the header of the document.}

  \item{before.body}{One or more files with content to be
  included before the document body.}

  \item{after.body}{One or more files with content to be
  included after the document body.}
}
\value{
A character vector with pandoc command line arguments
}
\description{
Functions that assist in creating various types of pandoc
command line options (e.g. for templates, table of
contents, highlighting, and content includes)
}
\details{
Non-absolute paths for resources referenced from the
\code{include.header}, \code{include.before}, and
\code{include.after} parameters are resolved relative to
the directory of the input document.
}

