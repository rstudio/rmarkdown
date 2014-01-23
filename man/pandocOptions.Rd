\name{pandocOptions}
\alias{pandocOptions}
\title{Pandoc conversion options}
\usage{
pandocOptions(options)
}
\arguments{
  \item{options}{An object that implements the
  \code{pandocOptions} S3 method.}
}
\value{
A character vector with pandoc command line arguments
}
\description{
Generic method to get pandoc command-line options from an
object (typically a list containing format-specific
options).
}

