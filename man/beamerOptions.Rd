\name{beamerOptions}
\alias{beamerOptions}
\title{Options for Beamer conversion}
\usage{
beamerOptions(..., toc = FALSE, slide.level = 2, incremental = FALSE,
  highlight = "default", includes = NULL)
}
\arguments{
  \item{\dots}{Command line options to pass to pandoc}

  \item{toc}{\code{TRUE} to include a table of contents in
  the output (only level 1 headers will be included in the
  table of contents).}

  \item{slide.level}{The heading level which defines
  indvidual slides. By default this is level 2, which
  allows level 1 headers to be used to define sections of
  the presentation.}

  \item{incremental}{\code{TRUE} to render slide bullets
  incrementally. Note that if you want to reverse the
  default incremental behavior for an individual bullet you
  can preceded it with \code{>}. For example: \emph{\code{>
  - Bullet Text}}}

  \item{highlight}{Syntax highlighting style. Supported
  styles include "default", "pygments", "kate",
  "monochrome", "espresso", "zenburn", "haddock", and
  "tango". Pass \code{NULL} to prevent syntax
  highlighting.}

  \item{includes}{Additional content to include within the
  document (typically created using the
  \code{\link[pandoc:includeOptions]{includeOptions}}
  function).}
}
\value{
A character vector of options that can be passed to
\code{\link{rmd2beamer}}.
}
\description{
Define the options for converting R Markdown to Beamer
}
\seealso{
\code{\link{rmd2beamer}}
}

