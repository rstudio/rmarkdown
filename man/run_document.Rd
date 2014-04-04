\name{run_document}
\alias{run_document}
\title{Run a Shiny document}
\usage{
run_document(input, shiny_args = NULL, ...)
}
\arguments{
  \item{input}{Input file}

  \item{shiny_args}{Arguments to the \pkg{shiny}
  \code{\link{runApp}} function.}

  \item{...}{Additional arguments to \code{\link{render}}.}
}
\value{
  Invisible NULL.
}
\description{
  Render a Shiny document and start Shiny to show the
  output.
}
\details{
  The \code{run_document} function fully renders an R
  Markdown document to HTML (using \code{\link{render}}),
  creates a Shiny application from the result, and then
  invokes \code{runApp} to view the application.

  Note that unlike \code{render}, \code{run_document} does
  not produce a file on disk that can be saved or shared.
}
\examples{
\dontrun{

# Render shiny_doc.Rmd and start a server for the result on port 8241.
rmarkdown::run_document("shiny_doc.Rmd", shiny_args = list(port = 8241))

}
}

