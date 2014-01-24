\name{knitRequired}
\alias{knitRequired}
\title{Check Whether an Input Document Requires Knitting}
\usage{
knitRequired(input)
}
\arguments{
  \item{input}{Input document}
}
\value{
Logical indicating whether the document requires knitting.
}
\description{
Check whether the input document requires knitting by
evaluating it's file extension. R Markdown (e.g. .Rmd)
requires knitting whereas plain markdown (e.g. .md) does
not.
}

