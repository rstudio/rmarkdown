\name{html_document}
\alias{html_document}
\title{Convert to an HTML document}
\usage{
html_document(toc = FALSE, toc.depth = 3, fig.width = 7, fig.height = 5,
  fig.retina = 2, fig.caption = FALSE, smart = TRUE,
  self.contained = TRUE, theme = "default", highlight = "default",
  mathjax = "default", template = NULL, css = NULL, includes = NULL,
  data.dir = NULL, pandoc.args = NULL)
}
\arguments{
  \item{toc}{\code{TRUE} to include a table of contents in
  the output}

  \item{toc.depth}{Depth of headers to include in table of
  contents}

  \item{fig.width}{Default width (in inches) for figures}

  \item{fig.height}{Default width (in inches) for figures}

  \item{fig.retina}{Scaling to perform for retina displays
  (defaults to 2, which currently works for all widely used
  retina displays). Note that this only takes effect if you
  are using knitr >= 1.5.21. Set to \code{NULL} to prevent
  retina scaling.}

  \item{fig.caption}{\code{TRUE} to render figures with
  captions}

  \item{smart}{Produce typographically correct output,
  converting straight quotes to curly quotes, --- to
  em-dashes, -- to en-dashes, and ... to ellipses.}

  \item{self.contained}{Produce a standalone HTML file with
  no external dependencies, using data: URIs to incorporate
  the contents of linked scripts, stylesheets, images, and
  videos. Note that if you specify "local" for
  \code{mathjax} then \code{self.contained} is
  automatically set to \code{FALSE}.}

  \item{theme}{Visual theme ("default", "cerulean",
  "journal", "flatly", "readable", "spacelab", "united",
  "yeti", or "cosmo"). Pass \code{NULL} for no theme (in
  this case you can use the \code{css} parameter to add
  your own styles).}

  \item{highlight}{Syntax highlighting style. Supported
  styles include "default", "tango", "pygments", "kate",
  "monochrome", "espresso", "zenburn", "haddock", and
  "textmate". Pass \code{NULL} to prevent syntax
  highlighting.}

  \item{mathjax}{Include mathjax. The "default" option uses
  an https URL from the official MathJax CDN. The "local"
  option uses a local version of MathJax (which is copied
  into the output directory). You can pass an alternate URL
  or pass \code{NULL} to exclude MathJax entirely.}

  \item{template}{Pandoc template to use for rendering
  HTML. See the Templates section below for more details on
  templates.}

  \item{css}{One or more css files to include}

  \item{includes}{Named list of additional content to
  include within the document (typically created using the
  \code{\link{includes}} function).}

  \item{data.dir}{Additional directory to resolve relatives
  paths of templates and included content against (the
  directory of the input file is used by default).}

  \item{pandoc.args}{Additional command line options to
  pass to pandoc}
}
\value{
R Markdown output format to pass to \code{\link{render}}
}
\description{
Format for converting from R Markdown to an HTML document.
}
\details{
R Markdown documents can have optional metadata that is
used to generate a document header that includes the title,
author, and date. Metadata can also be provided to enable
the use of footnotes and bibliographies. For more details
see the documentation on R Markdown
\link[=rmd_metadata]{metadata} and
\link[=rmd_citations]{citations}.
}
\section{Templates}{
  You can provide a custom HTML template to be used for
  rendering. The syntax for templates is described in the
  documentation on
  \href{http://johnmacfarlane.net/pandoc/demo/example9/templates.html}{pandoc
  templates}.

  Note however that if you choose to provide your own HTML
  template then several aspects of HTML document rendering
  will behave differently:

  \itemize{ \item{The \code{theme} parameter does not work
  (you can still provide styles using the \code{css}
  parameter).  } \item{For the \code{highlight} parameter,
  the default highlighting style will resolve to "tango"
  and the "textmate" highlighting style is not available }
  \item{MathJax is automatically disabled if
  \code{self.contained} is \code{TRUE} (these two options
  can't be used together in normal pandoc templates).  } }

  Due to the above restrictions, you might consider using
  the \code{includes} parameter as an alternative to
  providing a fully custom template.
}
\examples{
\dontrun{

library(rmarkdown)

render("input.Rmd", html_document())

render("input.Rmd", html_document(toc = TRUE))
}
}

