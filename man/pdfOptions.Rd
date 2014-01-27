\name{pdfOptions}
\alias{pdfOptions}
\title{Options for PDF conversion.}
\usage{
pdfOptions(..., toc = FALSE, toc.depth = 2, number.sections = FALSE,
  highlight = highlighter(), latex.engine = c("pdflatex", "lualatex",
  "xelatex"), natbib = FALSE, biblatex = FALSE, includes = NULL)
}
\arguments{
  \item{\dots}{Command line options to pass to pandoc}

  \item{toc}{\code{TRUE} to include a table of contents in
  the output}

  \item{toc.depth}{Depth of headers to include in table of
  contents}

  \item{number.sections}{\code{TRUE} Number section
  headings}

  \item{highlight}{Syntax highlighting style (see
  \code{\link{highlighter}}).  Pass \code{NULL} to prevent
  syntax highlighting.}

  \item{latex.engine}{LaTeX engine for producing PDF
  output. Options are pdflatex, lualatex, and xelatex.}

  \item{natbib}{Use natbib for citations in LaTeX output}

  \item{biblatex}{Use biblatex for citations in LaTeX
  output}

  \item{includes}{Additional content to include within the
  document (typically created using the
  \code{\link[pandoc:includeOptions]{includeOptions}}
  function).}
}
\value{
A character vector of PDF options that can be passed to
\code{\link{rmd2pdf}}.
}
\description{
Define the options for converting R Markdown to PDF.
}
\seealso{
\code{\link{rmd2pdf}}
}

