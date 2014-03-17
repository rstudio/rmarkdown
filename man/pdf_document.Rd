\name{pdf_document}
\alias{pdf_document}
\title{Convert to a PDF document}
\usage{
pdf_document(toc = FALSE, toc_depth = 2, number_sections = FALSE,
  fig_width = 6.5, fig_height = 4.5, fig_crop = TRUE,
  fig_caption = FALSE, highlight = "default", template = "default",
  keep_tex = FALSE, latex_engine = "pdflatex", natbib = FALSE,
  biblatex = FALSE, includes = NULL, data_dir = NULL,
  pandoc_args = NULL)
}
\arguments{
  \item{number_sections}{\code{TRUE} to number section
  headings}

  \item{fig_crop}{\code{TRUE} to automatically apply the
  \code{pdfcrop} utility (if available) to pdf figures}

  \item{highlight}{Syntax highlighting style. Supported
  styles include "default", "tango", "pygments", "kate",
  "monochrome", "espresso", "zenburn", and "haddock". Pass
  \code{NULL} to prevent syntax highlighting.}

  \item{keep_tex}{Keep the intermediate tex file used in
  the conversion to PDF}

  \item{latex_engine}{LaTeX engine for producing PDF
  output. Options are "pdflatex", "lualatex", and
  "xelatex".}

  \item{natbib}{Use natbib for citations in LaTeX output}

  \item{biblatex}{Use biblatex for citations in LaTeX
  output}

  \item{template}{Pandoc template to use for rendering.
  Pass "default" to use the rmarkdown package default
  template; pass \code{NULL} to use pandoc's built-in
  template; pass a path to use a custom template that
  you've created.  See the documentation on
  \href{http://johnmacfarlane.net/pandoc/demo/example9/templates.html}{pandoc
  templates} for more details.}

  \item{toc}{\code{TRUE} to include a table of contents in
  the output}

  \item{toc_depth}{Depth of headers to include in table of
  contents}

  \item{fig_width}{Default width (in inches) for figures}

  \item{fig_height}{Default width (in inches) for figures}

  \item{fig_caption}{\code{TRUE} to render figures with
  captions}

  \item{includes}{Named list of additional content to
  include within the document (typically created using the
  \code{\link{includes}} function).}

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
with xelatex and lualatex, see the \code{latex_engine}
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
render("input.Rmd", pdf_document(latex_engine = "lualatex"))

# add a table of contents and pass an option to pandoc
render("input.Rmd", pdf_document(toc = TRUE, "--listings"))
}
}

