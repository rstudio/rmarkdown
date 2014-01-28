\name{pdf_document}
\alias{pdf_document}
\title{Convert to a PDF document}
\usage{
pdf_document(..., toc = FALSE, toc.depth = 2, number.sections = FALSE,
  fig.width = 6, fig.height = 4.5, highlight = "default",
  latex.engine = "pdflatex", natbib = FALSE, biblatex = FALSE,
  includes = NULL)
}
\arguments{
  \item{toc}{\code{TRUE} to include a table of contents in
  the output}

  \item{toc.depth}{Depth of headers to include in table of
  contents}

  \item{number.sections}{\code{TRUE} Number section
  headings}

  \item{fig.width}{Default width (in inches) for figures}

  \item{fig.height}{Default width (in inches) for figures}

  \item{highlight}{Syntax highlighting style. Supported
  styles include "default", "pygments", "kate",
  "monochrome", "espresso", "zenburn", "haddock", and
  "tango". Pass \code{NULL} to prevent syntax highlighting.
  Pass \code{NULL} to prevent syntax highlighting.}

  \item{latex.engine}{LaTeX engine for producing PDF
  output. Options are "pdflatex", "lualatex", and
  "xelatex".}

  \item{natbib}{Use natbib for citations in LaTeX output}

  \item{biblatex}{Use biblatex for citations in LaTeX
  output}

  \item{includes}{Additional content to include within the
  document (typically created using the
  \code{\link[pandoc:include_options]{include_options}}
  function).}

  \item{\dots}{Command line options to pass to pandoc}
}
\value{
R Markdown output format to pass to \code{\link{render}}
}
\description{
Format for converting from R Markdown to a PDF document.
}
\details{
Creating PDF output from R Markdown requires that LaTeX be
installed.

R Markdown documents can have optional metadata that is
used to generate a document header that includes the title,
author, and date. Metadata can also be provided to enable
the use of footnotes and bibliographies. For more details
see the documentation on R Markdown
\link[=rmd_metadata]{metadata} and
\link[=rmd_citations]{citations}.

Many aspects of the LaTeX template used to create PDF
documents can be customized using metadata. For example:

\tabular{l}{ \code{---} \cr \code{title: "Crop Analysis Q3
2013"} \cr \code{fontsize: 11pt} \cr \code{geometry:
margin=1in} \cr \code{---} }

Available metadata variables include:

\describe{ \item{\code{lang}}{Document language code}
\item{\code{fontsize}}{Font size (e.g. 10pt, 11pt, 12pt)}
\item{\code{documentclass}}{LaTeX document class (e.g.
article)} \item{\code{classoption}}{Option for
\code{documentclass} (e.g. oneside); may be repeated}
\item{\code{geometry}}{Options for geometry class (e.g.
margin=1in); may be repeated} \item{\code{mainfont,
sansfont, monofont, mathfont}}{Document fonts (works only
with xelatex and lualatex, see the \code{latex.engine}
option)} \item{\code{linkcolor, urlcolor, citecolor}}{Color
for internal, external, and citation links (red, green,
magenta, cyan, blue, black)}
\item{\code{biblio-style}}{LaTeX bibliography style (used
with \code{natbib} option)}
\item{\code{biblio-files}}{Bibliography files to use in
LaTeX (used with \code{natbib} or \code{biblatex} options)}
}
}
\examples{
\dontrun{

library(rmarkdown)

# simple invocation
render("input.Rmd", pdf_document())

# specify an option for latex engine
render("input.Rmd", pdf_document(latex.engine = "lualatex"))

# add a table of contents and pass an option to pandoc
render("input.Rmd", pdf_document(toc = TRUE, "--listings"))
}
}

