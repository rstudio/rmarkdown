\name{html_document}
\alias{html_document}
\title{Convert to an HTML document}
\usage{
html_document(toc = FALSE, toc.depth = 3, fig.width = 7, fig.height = 5,
  theme = "default", highlight = "default", mathjax = "default",
  css = NULL, includes = NULL, pandoc.args = NULL)
}
\arguments{
  \item{toc}{\code{TRUE} to include a table of contents in
  the output}

  \item{toc.depth}{Depth of headers to include in table of
  contents}

  \item{fig.width}{Default width (in inches) for figures}

  \item{fig.height}{Default width (in inches) for figures}

  \item{theme}{Visual theme ("default", "cerulean", or
  "slate"). Pass \code{NULL} for no theme (in which case
  you want to pass some custom CSS using the \code{css}
  parameter)}

  \item{highlight}{Syntax highlighting style. Supported
  styles include "default", "pygments", "kate",
  "monochrome", "espresso", "zenburn", "haddock", and
  "tango". Pass \code{NULL} to prevent syntax
  highlighting.}

  \item{mathjax}{Include mathjax. The "default" option uses
  an https URL from the official MathJax CDN. You can pass
  an alternate URL or pass \code{NULL} to exclude MathJax
  entirely.}

  \item{css}{One or more css files to include}

  \item{includes}{Additional content to include within the
  document (typically created using the
  \code{\link[pandoc:include_options]{include_options}}
  function).}

  \item{pandoc.args}{Additional command line options to
  pass to pandoc}
}
\value{
R Markdown output format to pass to \code{\link{render}}
}
\description{
Format for converting from R Markdown to an HTML document.
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

render("input.Rmd", html_document())

render("input.Rmd", html_document(toc = TRUE))
}
}

