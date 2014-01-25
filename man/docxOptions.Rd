\name{docxOptions}
\alias{docxOptions}
\title{Options for MS Word conversion}
\usage{
docxOptions(..., highlight = highlighter(), reference.docx = NULL)
}
\arguments{
  \item{\dots}{Command line options to pass to pandoc}

  \item{highlight}{Syntax highlighting style (see
  \code{\link{highlighter}}).  Pass \code{NULL} to prevent
  syntax highlighting.}

  \item{reference.docx}{Use the specified file as a style
  reference in producing a docx file. For best results, the
  reference docx should be a modified version of a docx
  file produced using pandoc.}
}
\value{
A character vector of options that can be passed to
\code{\link{rmd2docx}}.
}
\description{
Define the options for converting R Markdown to MS Word
docx
}
\seealso{
\code{\link{rmd2docx}}
}

