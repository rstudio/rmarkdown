\name{pdfOptions}
\alias{pdfOptions}
\title{Options for PDF conversion}
\usage{
  pdfOptions(toc = FALSE, toc.depth = 2,
    number.sections = FALSE, geometry = c(margin = "1in"),
    highlight = "default", include.header = NULL,
    include.before = NULL, include.after = NULL)
}
\arguments{
  \item{toc}{\code{TRUE} to include a table of contents in
  the output}

  \item{toc.depth}{Depth of headers to include in table of
  contents}

  \item{number.sections}{\code{TRUE} Number section
  headings}

  \item{geometry}{Named LaTeX geometry options for the
  document.}

  \item{highlight}{Style for syntax highlighting. Options
  are default, pygments, kate, monochrome, espresso,
  zenburn, haddock, and tango. Pass \code{NULL} to prevent
  syntax highlighting.}

  \item{include.header}{One or more files with LaTeX
  content to be included in the header of the document.}

  \item{include.before}{One or more files with LaTeX
  content to be included before the document body.}

  \item{include.after}{One or more files with LaTeX content
  to be included after the document body.}
}
\value{
  A list of PDF options that can be passed to
  \code{\link{rmd2pdf}}.
}
\description{
  Define the options for converting R Markdown to PDF.
}
\details{
  Paths for resources referenced from the
  \code{include.header}, \code{include.before}, and
  \code{include.after} parameters are resolved relative to
  the directory of the input document.
}

