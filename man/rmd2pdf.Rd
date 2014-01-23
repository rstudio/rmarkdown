\name{rmd2pdf}
\alias{rmd2pdf}
\title{Convert R Markdown to PDF}
\usage{
rmd2pdf(input, options = pdfOptions(), output = NULL,
  envir = parent.frame(), quiet = FALSE, encoding = getOption("encoding"))
}
\arguments{
  \item{input}{Input Rmd document}

  \item{options}{Character vector of pandoc options created
  by calling \code{\link{pdfOptions}}}

  \item{output}{Target output file (defaults to <input>.pdf
  if not specified)}

  \item{envir}{The environment in which the code chunks are
  to be evaluated (can use \code{\link{new.env}()} to
  guarantee an empty new environment)}

  \item{quiet}{Whether to suppress the progress bar and
  messages}

  \item{encoding}{The encoding of the input file; see
  \code{\link{file}}}
}
\value{
The compiled document is written into the output file, and
the path of the output file is returned.
}
\description{
Convert an R Markdown (Rmd) file to PDF. The document is
\code{\link[knitr:knit]{knit}} and then converted to PDF
using
\href{http://johnmacfarlane.net/pandoc/index.html}{pandoc}.
}
\section{Metadata}{
  Rmd files include a metadata section (typically located
  at the top of the file) that include title, author, and
  date information as well additional variables used to
  customize document generation. Here is an example
  metadata section:

  \tabular{l}{ \code{---} \cr \code{title: "Crop Analysis
  Q3 2013"} \cr \code{author: Martha Smith} \cr \code{date:
  October 23rd, 2013} \cr \code{fontsize: 11pt} \cr
  \code{geometry: margin=1in} \cr \code{---} }

  In addition to the options specified by
  \code{\link{pdfOptions}}, many other aspects of the LaTeX
  template used to create PDF output can be customized
  using metadata. Available variables include:

  \describe{ \item{\code{lang}}{Document language code}
  \item{\code{fontsize}}{Font size (e.g. 10pt, 11pt, 12pt)}
  \item{\code{documentclass}}{LaTeX document class (e.g.
  article)} \item{\code{classoption}}{Option for
  \code{documentclass} (e.g. oneside); may be repeated}
  \item{\code{geometry}}{Options for geometry class (e.g.
  margin=1in); may be repeated} \item{\code{mainfont,
  sansfont, monofont, mathfont}}{Document fonts (works only
  with xelatex and lualatex, see the \code{latex.engine}
  option)} \item{\code{linkcolor, urlcolor,
  citecolor}}{Color for internal, external, and citation
  links (red, green, magenta, cyan, blue, black)}
  \item{\code{biblio-style}}{LaTeX bibliography style (used
  with \code{natbib} option)}
  \item{\code{biblio-files}}{Bibliography files to use in
  LaTeX (used with \code{natbib} or \code{biblatex}
  options)} }
}

\section{Citations}{
  R Markdown documents can also include footnotes and
  citations, with support for a wide variety of
  bibliography formats and output styles. To define the
  bibliography and citation styles for a document you add
  the \code{bibliography} and \code{csl} metadata fields.
  For example:

  \tabular{l}{ \code{---} \cr \code{title: "Crop Analysis
  Q3 2013"} \cr \code{bibliography: crop-analysis.bib} \cr
  \code{csl: chicago-author-date.csl} \cr \code{---} }

  Note that the referenced bibliography and csl files
  should be located in the same directory as your R
  Markdown document.

  You can find more information on the markdown syntax for
  citations within the pandoc documentation on
  \href{http://johnmacfarlane.net/pandoc/README.html#footnotes}{footnotes}
  and
  \href{http://johnmacfarlane.net/pandoc/README.html#citations}{citations}.
}
\seealso{
\code{\link[knitr:knit]{knit}}, \code{\link{pdfOptions}}
}

