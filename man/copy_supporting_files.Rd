\name{copy_supporting_files}
\alias{copy_supporting_files}
\title{Copy supporting files for an input document}
\usage{
copy_supporting_files(input, path, rename.to = NULL)
}
\arguments{
  \item{input}{Input document to copy supporting files for}

  \item{path}{Directory to copy files from}

  \item{rename.to}{Optional rename of source directory
  after it is copied}
}
\value{
The relative path to the supporting files. This path is
suitable for inclusion in HTML\code{href} and \code{src}
attributes.
}
\description{
Copy required supporting files for an input document to the
_files directory associated with the document.
}

