\name{render_supporting_files}
\alias{render_supporting_files}
\title{Render supporting files for an input document}
\usage{
render_supporting_files(from, to, rename_to = NULL)
}
\arguments{
  \item{from}{Directory to copy from}

  \item{to}{Directory to copy files into}

  \item{rename_to}{Optional rename of source directory
  after it is copied}
}
\value{
The relative path to the supporting files. This path is
suitable for inclusion in HTML\code{href} and \code{src}
attributes.
}
\description{
Render (copy) required supporting files for an input
document to the _files directory associated with the
document.
}

