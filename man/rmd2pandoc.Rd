\name{rmd2pandoc}
\alias{rmd2pandoc}
\title{Convert R Markdown using pandoc}
\usage{
rmd2pandoc(input, to, options = NULL, output = NULL,
  envir = parent.frame(), quiet = FALSE, encoding = getOption("encoding"))
}
\arguments{
  \item{input}{Input file (Rmd or plain markdown)}

  \item{to}{Pandoc format to convert to}

  \item{options}{Character vector of command line options
  to pass to pandoc.}

  \item{output}{Output file (if not specified then a
  default based on the specified \code{to} format is
  chosen)}

  \item{envir}{The environment in which the code chunks are
  to be evaluated during knitting (can use
  \code{\link{new.env}()} to guarantee an empty new
  environment)}

  \item{quiet}{\code{TRUE} to supress printing of the
  pandoc command line}

  \item{encoding}{the encoding of the input file; see
  \code{\link{file}}}
}
\value{
The compiled document is written into the output file, and
the path of the output file is returned.
}
\description{
Convert the input file using pandoc. If the input requires
knitting then \code{\link[knitr:knit]{knit}} is called
prior to pandoc.
}
\details{
Typically one of the \code{\link{knitrRender}} functions is
called prior to calling \code{knit2pandoc} to optimize
knitr rendering for the intended output format.
}
\section{R Markdown}{
  R Markdown supports all of the base pandoc markdown
  features as well as some optional features for
  compatibility with GitHub Flavored Markdown (which
  previous versions of R Markdown were based on).

  For more on pandoc markdown see the
  \href{http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html}{pandoc
  markdown specification}. Optional pandoc markdown
  features enabled include \code{autolink_bare_uris},
  \code{ascii_identifiers}, and
  \code{tex_math_single_backslash}.
}

