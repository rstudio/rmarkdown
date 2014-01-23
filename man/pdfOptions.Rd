\name{pdfOptions}
\alias{pdfOptions}
\title{Options for PDF conversion}
\usage{
pdfOptions(..., toc = FALSE, toc.depth = 2, number.sections = FALSE,
  geometry = c(margin = "1in"), highlight = "default", includes = NULL)
}
\arguments{
  \item{\dots}{Command line options to pass to pandoc}

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

  \item{includes}{Additional content to include within the
  document (typically created using the
  \code{\link{includeOptions}} function).}
}
\value{
A character vector of PDF options that can be passed to
\code{\link{rmd2pdf}}.
}
\description{
Define the options for converting R Markdown to PDF.
}

