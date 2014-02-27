\name{word_document}
\alias{word_document}
\title{Convert to an MS Word document}
\usage{
word_document(fig_width = 5, fig_height = 4, fig_caption = FALSE,
  highlight = "default", reference_docx = "default", data_dir = NULL,
  pandoc_args = NULL)
}
\arguments{
  \item{reference_docx}{Use the specified file as a style
  reference in producing a docx file. For best results, the
  reference docx should be a modified version of a docx
  file produced using pandoc. Pass "default" to use the
  rmarkdown default styles.}

  \item{fig_width}{Default width (in inches) for figures}

  \item{fig_height}{Default width (in inches) for figures}

  \item{fig_caption}{\code{TRUE} to render figures with
  captions}

  \item{highlight}{Syntax highlighting style. Supported
  styles include "default", "tango", "pygments", "kate",
  "monochrome", "espresso", "zenburn", and "haddock". Pass
  \code{NULL} to prevent syntax highlighting.}

  \item{data_dir}{Additional directory to resolve relatives
  paths of templates and included content against (the
  directory of the input file is used by default).}

  \item{pandoc_args}{Additional command line options to
  pass to pandoc}
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

