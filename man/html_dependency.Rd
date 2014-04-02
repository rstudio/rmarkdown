\name{html_dependency}
\alias{html_dependency}
\title{Define an HTML dependency}
\usage{
html_dependency(name, version, path, meta = NULL, script = NULL,
  stylesheet = NULL, head = NULL)
}
\arguments{
  \item{name}{Library name}

  \item{version}{Library version}

  \item{path}{Full path to library}

  \item{meta}{Named list of meta tags to insert into
  document head}

  \item{script}{Script(s) to include within the document
  head (should be specified relative to the \code{path}
  parameter).}

  \item{stylesheet}{Stylesheet(s) to include within the
  document (should be specified relative to the \code{path}
  parameter).}

  \item{head}{Arbitrary lines of HTML to insert into the
  document head}
}
\value{
An object that can be included in the list of dependencies
passed to \code{\link{html_output}}.
}
\description{
Define an HTML dependency (e.g. CSS or Javascript and
related library). HTML dependency definitions are required
for \code{\link{html_output}} that require CSS or
JavaScript within the document head to render correctly.
}
\details{
See the documentation on
\href{http://rmarkdown.rstudio.com/developer_custom_html_output.html}{R
Markdown Custom HTML Output} for examples and additional
details.
}

