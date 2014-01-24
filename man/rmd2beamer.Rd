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
\section{Metadata}{
  Rmd files include a metadata section (typically located
  at the top of the file) that include title, author, and
  date information as well additional variables used to
  customize document generation. Here is an example
  metadata section:

  \tabular{l}{ \code{---} \cr \code{title: "Crop Analysis
  Q3 2013"} \cr \code{author: Martha Smith} \cr \code{date:
  October 23rd, 2013} \cr \code{theme: Antibes} \cr
  \code{colortheme: crane} \cr \code{---} }

  In addition to the options specified by
  \code{\link{beamerOptions}}, many other aspects of the
  Beamer template can be customized using metadata.
  Available variables include:

  \describe{ \item{\code{lang}}{Document language code}
  \item{\code{theme}}{Beamer theme (e.g. "AnnArbor")}
  \item{\code{colortheme}}{Beamer color theme (e.g.
  "dolphin")} \item{\code{fonttheme}}{Beamer font theme
  (e.g. "structurebold")} \item{\code{biblio-style}}{LaTeX
  bibliography style (used with \code{natbib} option)}
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
\code{\link[knitr:knit]{knit}},
\code{\link{beamerOptions}},
}

