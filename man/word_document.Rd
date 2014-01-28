\name{word_document}
\alias{word_document}
\title{Convert to an MS Word document}
\usage{
word_document(..., highlight = "default", reference.docx = NULL)
}
\arguments{
  \item{highlight}{Syntax highlighting style. Supported
  styles include "default", "pygments", "kate",
  "monochrome", "espresso", "zenburn", "haddock", and
  "tango". Pass \code{NULL} to prevent syntax
  highlighting.}

  \item{reference.docx}{Use the specified file as a style
  reference in producing a docx file. For best results, the
  reference docx should be a modified version of a docx
  file produced using pandoc.}

  \item{\dots}{Command line options to pass to pandoc}
}
\value{
R Markdown output format to pass to \code{\link{render}}
}
\description{
Format for converting from R Markdown to an MS Word
document.
}
\details{
R Markdown documents can have optional metadata that is
used to generate a document header that includes the title,
author, and date. Metadata can also be provided to enable
the use of footnotes and bibliographies. For more details
see the documentation on R Markdown
\link[=rmd_metadata]{metadata} and
\link[=rmd_citations]{citations}.
}
\examples{
\dontrun{

library(rmarkdown)

# simple invocation
render("input.Rmd", word_document())

# specify an option for syntax highlighting
render("input.Rmd", word_document(highlight = "zenburn"))
}
}

