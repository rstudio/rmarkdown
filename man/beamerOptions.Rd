\name{beamerOptions}
\alias{beamerOptions}
\title{Options for Beamer conversion}
\usage{
  beamerOptions(toc = FALSE, slide.level = 2,
    incremental = FALSE, highlight = "default",
    include.header = NULL, include.before = NULL,
    include.after = NULL)
}
\arguments{
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

  \item{highlight}{Style for syntax highlighting. Options
  are default, pygments, kate, monochrome, espresso,
  zenburn, haddock, and tango. Pass \code{NULL} to prevent
  syntax highlighting.}

  \item{include.header}{One or more files with LaTeX
  content to be included in the header of the document.}

  \item{include.before}{One or more files with LaTeX
  content to be included before the document body.}

  \item{include.after}{One or more files with LaTeX content
  to be included after the document body.}
}
\value{
  A list of options that can be passed to
  \code{\link{rmd2beamer}}.
}
\description{
  Define the options for converting R Markdown to Beamer
}
\details{
  Paths for resources referenced from the
  \code{include.header}, \code{include.before}, and
  \code{include.after} parameters are resolved relative to
  the directory of the input document.
}

