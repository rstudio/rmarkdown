\name{rmd2pdf}
\alias{rmd2pdf}
\title{Convert R Markdown to PDF}
\usage{
rmd2pdf(input, options = pdfOptions(), output = NULL,
  envir = parent.frame(), quiet = FALSE, encoding = getOption("encoding"))
}
\arguments{
  \item{input}{Input file (Rmd or plain markdown)}

  \item{options}{Character vector of pandoc options created
  by calling \code{\link{pdfOptions}}}

  \item{output}{Target output file (defaults to <input>.pdf
  if not specified)}

  \item{envir}{The environment in which the code chunks are
  to be evaluated during knitting (can use
  \code{\link{new.env}()} to guarantee an empty new
  environment)}

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
Converts the input file to PDF using pandoc. If the input
requires knitting then \code{\link[knitr:knit]{knit}} is
called prior to pandoc.
}
\details{
Creating PDF output from R Markdown requires that LaTeX be
installed.

R Markdown documents can have optional metadata that is
used to generate a document header that includes the title,
author, and date. Metadata can also be provided to enable
the use of footnotes and bibliographies. For more details
see the documentation on R Markdown
\link[=rmdMetadata]{metadata} and
\link[=rmdCitations]{citations}.

In addition to the options specified by
\code{\link{pdfOptions}}, many other aspects of the LaTeX
template used to create PDF output can be customized using
metadata. For example:

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
rmd2pdf("input.Rmd")

# specify an option for latex engine
rmd2pdf("input.Rmd", pdfOptions(latex.engine = "lualatex"))

# add a table of contents and pass an option to pandoc
rmd2pdf("input.Rmd", pdfOptions(toc = TRUE, "--listings"))
}
}
\seealso{
\code{\link[knitr:knit]{knit}}, \code{\link{pdfOptions}}
}

