\name{htmlOptions}
\alias{htmlOptions}
\alias{mathjaxURL}
\title{Options for HTML Conversion}
\usage{
htmlOptions(toc = FALSE, toc.depth = 3, theme = "default",
  highlight = "default", mathjax = mathjaxURL(), css = NULL,
  includes = NULL)

mathjaxURL()
}
\arguments{
  \item{toc}{\code{TRUE} to include a table of contents in
  the output}

  \item{toc.depth}{Depth of headers to include in table of
  contents}

  \item{theme}{Visual theme ("default", "cerulean", or
  "slate"). Pass \code{NULL} for no theme (in which case
  you want to pass some custom CSS using the \code{css}
  parameter)}

  \item{highlight}{Style for syntax highlighting. Options
  are default, pygments, kate, monochrome, espresso,
  zenburn, haddock, and tango. Pass \code{NULL} to prevent
  syntax highlighting.}

  \item{mathjax}{Include mathjax from the specified URL.
  Pass \code{NULL} to not include mathjax.}

  \item{css}{One or more css files to include}

  \item{includes}{Additional content to include within the
  document (typically created using the
  \code{\link{includeOptions}} function).}
}
\value{
A character vector of HTML options that can be passed to
\code{\link{rmd2html}}.
}
\description{
Define the options for converting R Markdown to HTML.
}

