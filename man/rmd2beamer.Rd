\name{rmd2beamer}
\alias{rmd2beamer}
\title{Convert R Markdown to Beamer}
\usage{
rmd2beamer(input, options = beamerOptions(), output = NULL,
  envir = parent.frame(), quiet = FALSE, encoding = getOption("encoding"))
}
\arguments{
  \item{input}{Input file (Rmd or plain markdown)}

  \item{options}{Character vector of pandoc options created
  by calling \code{\link{beamerOptions}}}

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
Converts the input file to Beamer using pandoc. If the
input requires knitting then \code{\link[knitr:knit]{knit}}
is called prior to pandoc.
}
\details{
For more information on markdown syntax for presentations
see
\href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
slide shows with pandoc}.

When including figures generated from R plots within Beamer
slides it's likely you'll need to tweak their sizes for
optimal results. You can do this using the \code{fig.width}
and \code{fig.height} chunk options. If you find that there
is too much space around a plot (common with base graphics)
you can apply the pdfcrop filter by adding the \code{crop =
TRUE} chunk option.

R Markdown documents can have optional metadata that is
used to generate a document header that includes the title,
author, and date. Metadata can also be provided to enable
the use of footnotes and bibliographies. For more details
see the documentation on R Markdown
\link[=rmdMetadata]{metadata} and
\link[=rmdCitations]{citations}.

In addition to the options specified by
\code{\link{beamerOptions}}, many other aspects of the
LaTeX template used to create Beamer output can be
customized using metadata. For example:

\tabular{l}{ \code{---} \cr \code{title: "Crop Analysis Q3
2013"} \cr \code{theme: AnnArbor} \cr \code{colortheme:
dolphin} \cr \code{---} }

Available metadata variables include:

\describe{ \item{\code{lang}}{Document language code}
\item{\code{theme}}{Beamer theme (e.g. "AnnArbor")}
\item{\code{colortheme}}{Beamer color theme (e.g.
"dolphin")} \item{\code{fonttheme}}{Beamer font theme (e.g.
"structurebold")} \item{\code{biblio-style}}{LaTeX
bibliography style (used with \code{natbib} option)}
\item{\code{biblio-files}}{Bibliography files to use in
LaTeX (used with \code{natbib} or \code{biblatex} options)}
}
}
\examples{
\dontrun{

library(rmarkdown)

# simple invocation
rmd2beamer("pres.Rmd")

# specify an option for incremental rendering
rmd2beamer("pres.Rmd", beamerOptions(incremental = TRUE))
}
}
\seealso{
\code{\link[knitr:knit]{knit}}, \code{\link{beamerOptions}}
}

