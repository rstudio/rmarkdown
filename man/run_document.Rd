\name{run_document}
\alias{run_document}
\title{Run a Shiny document}
\usage{
run_document(file, auto_reload = TRUE, shiny_args = NULL, ...)
}
\arguments{
  \item{file}{Input file}

  \item{auto_reload}{If \code{TRUE} (the default),
  automatically reload the Shiny application when the input
  file is changed.}

  \item{shiny_args}{Arguments to the \code{\link{runApp}}
  function.}

  \item{...}{Additional arguments to \code{\link{render}}.}
}
\value{
  Invisible NULL.
}
\description{
  Start a Shiny server for the given document, and render
  it for display.
}
\details{
  The \code{run_document} function runs a Shiny document by
  starting a \pkg{shiny} server associated with the
  document, and rendering the document as necessary when
  connections to the server are made.
}
\note{
  Unlike \code{\link{render}}, \code{run_document} does not
  render the document to a file on disk. To view the
  document, point a Web browser to the URL displayed when
  the server starts. In most cases a Web browser will be
  started automatically to view the document; see
  \code{launch.browser} in the \code{\link{runApp}}
  documentation for details.
}
\examples{
\dontrun{

# Run the Shiny document "shiny_doc.Rmd" on port 8241

rmarkdown::run_document("shiny_doc.Rmd", shiny_args = list(port = 8241))

}
}

