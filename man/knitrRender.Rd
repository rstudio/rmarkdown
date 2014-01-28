\name{knitrRender}
\alias{knitrRender}
\alias{knitrRenderDocx}
\alias{knitrRenderHtml}
\alias{knitrRenderPdf}
\title{Set knitr hooks and options for rendering R Markdown}
\usage{
knitrRender(format)

knitrRenderDocx(fig.width, fig.height)

knitrRenderHtml(format, fig.width, fig.height)

knitrRenderPdf(format, fig.width, fig.height)
}
\arguments{
  \item{format}{Pandoc format being rendered}

  \item{fig.width}{Default width (in inches) for figures}

  \item{fig.height}{Default height (in inches) for figures}
}
\description{
These functions set knitr hooks and options for markdown
rendering. Hooks are based on the default
\code{\link[knitr:render_markdown]{knitr::render_markdown}}
function with additional customization for various output
formats.
}
\details{
Some file formats like HTML and PDF have multiple potential
pandoc formats (e.g. PDF can be either "latex" or "beamer"
and HTML can be "html", "html5", "revealjs", etc.). The
format argument is used to distinguish among these, as well
as ensure that distinct formats have their own knitr figure
directories.

You typically need to call only one knitr render function,
as the various format-specific functions (e.g.
\code{knitrRenderPdf}) all call the \code{knitrRender}
function as part of their implementation.
}

