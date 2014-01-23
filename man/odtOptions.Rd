\name{odtOptions}
\alias{odtOptions}
\title{Options for OpenDocument conversion}
\usage{
odtOptions(..., highlight = "default", reference.odt = NULL)
}
\arguments{
  \item{\dots}{Command line options to pass to pandoc}

  \item{highlight}{Style for syntax highlighting. Options
  are default, pygments, kate, monochrome, espresso,
  zenburn, haddock, and tango. Pass \code{NULL} to prevent
  syntax highlighting.}

  \item{reference.odt}{Use the specified file as a style
  reference in producing an odt file. For best results, the
  reference odt should be a modified version of a odt file
  produced using pandoc.}
}
\value{
A character vector of options that can be passed to
\code{\link{rmd2odt}}.
}
\description{
Define the options for converting R Markdown to
OpenDocument odt
}

