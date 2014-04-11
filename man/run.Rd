\name{run}
\alias{run}
\title{Run a Shiny document}
\usage{
run(file, auto_reload = TRUE, shiny_args = NULL, render_args = NULL)
}
\arguments{
  \item{file}{Input file}

  \item{auto_reload}{If \code{TRUE} (the default),
  automatically reload the Shiny application when the input
  file is changed.}

  \item{shiny_args}{Additional arguments to
  \code{\link[shiny:runApp]{runApp}}.}

  \item{render_args}{Additional arguments to
  \code{\link{render}}.}
}
\value{
Invisible NULL.
}
\description{
Start a Shiny server for the given document, and render it
for display.
}
\details{
The \code{run} function runs a Shiny document by starting a
Shiny server associated with the document. The
\code{shiny_args} parameter can be used to configure the
server; see the \code{\link[shiny:runApp]{runApp}}
documentation for details.

Once the server is started, the document will be rendered
using \code{\link{render}}. The server will initiate a
render of the document whenever necessary, so it is not
necessary to call \code{run} every time the document
changes: if \code{auto_reload} is \code{TRUE}, saving the
document will trigger a render. You can also manually
trigger a render by reloading the document in a Web
browser.
}
\note{
Unlike \code{\link{render}}, \code{run} does not render the
document to a file on disk. To view the document, point a
Web browser to the URL displayed when the server starts. In
most cases a Web browser will be started automatically to
view the document; see \code{launch.browser} in the
\code{\link[shiny:runApp]{runApp}} documentation for
details.
}
\examples{
\dontrun{

# Run the Shiny document "shiny_doc.Rmd" on port 8241

rmarkdown::run("shiny_doc.Rmd", shiny_args = list(port = 8241))

}
}

