\name{rmd2docx}
\alias{rmd2docx}
\title{Convert R Markdown to MS Word}
\usage{
rmd2docx(input, options = docxOptions(), output = NULL,
  envir = parent.frame(), quiet = FALSE, encoding = getOption("encoding"))
}
\arguments{
  \item{input}{input Rmd document}

  \item{options}{Character vector of pandoc options created
  by calling \code{\link{docxOptions}}}

  \item{output}{Target output file (defaults to
  <input>.docx if not specified)}

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
Converts an R Markdown (Rmd) file to an MS Word docx. The
document is \code{\link[knitr:knit]{knit}} and then
converted to docx using
\href{http://johnmacfarlane.net/pandoc/index.html}{pandoc}.
}
\section{Metadata}{
  Rmd files include a metadata section (typically located
  at the top of the file) that include title, author, and
  date information. Here is an example metadata section:

  \tabular{l}{ \code{---} \cr \code{title: "Crop Analysis
  Q3 2013"} \cr \code{author: Martha Smith} \cr \code{date:
  October 23rd, 2013} \cr \code{---} }
}
\seealso{
\code{\link[knitr:knit]{knit}}, \code{\link{docxOptions}},
}

