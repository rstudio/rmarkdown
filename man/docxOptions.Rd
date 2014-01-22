\name{docxOptions}
\alias{docxOptions}
\title{Options for MS Word conversion}
\usage{
  docxOptions(highlight = "default", reference.docx = NULL)
}
\arguments{
  \item{highlight}{Style for syntax highlighting. Options
  are default, pygments, kate, monochrome, espresso,
  zenburn, haddock, and tango. Pass \code{NULL} to prevent
  syntax highlighting.}

  \item{reference.docx}{Use the specified file as a style
  reference in producing a docx file. For best results, the
  reference docx should be a modified version of a docx
  file produced using pandoc.}
}
\value{
  A list of options that can be passed to
  \code{\link{rmd2docx}}.
}
\description{
  Define the options for converting R Markdown to MS Word
  docx
}

