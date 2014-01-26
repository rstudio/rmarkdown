\docType{package}
\name{rmarkdown-package}
\alias{rmarkdown}
\alias{rmarkdown-package}
\title{R Markdown Document Conversion}
\description{
  The \pkg{rmarkdown} package is a next generation implementation of R Markdown based on \href{http://johnmacfarlane.net/pandoc/}{pandoc}. This implementation brings many enhancements to R Markdown, including:

\itemize{
  \item A wide variety of output formats including HTML, PDF, MS Word, and Beamer.
  \item New markdown syntax including expanded support for tables, definition lists, and citations.
  \item A responsive (multi-device friendly) HTML template based on \href{http://getbootstrap.com}{Bootstrap 3}.
  \item The ability to include raw LaTeX and LaTeX macros within markdown for advanced customization of PDF output.
  \item Extensive hooks for customizing HTML and PDF output.
}

}

\details{

These functions knit the specified input document and then produce the final output document using pandoc:

\preformatted{rmd2html("input.Rmd")
rmd2pdf("input.Rmd")
}

You can also specify a plain markdown file in which case knitting will be bypassed:

\preformatted{rmd2html("input.md")
}

All of the output formats have a corresponding options function which can be used to customize the ouptut, for example:

\preformatted{rmd2html("input.Rmd", htmlOptions(toc = TRUE))
rmd2pdf("input.Rmd", pdfOptions(latex.engine = "lualatex"))
rmd2beamer("input.Rmd", beamerOptions(incremental = TRUE))
}

You can also include arbitrary pandoc command line arguments in the call to the options function:

\preformatted{rmd2pdf("input.Rmd", pdfOptions(toc = TRUE, "--listings"))
}

}

\seealso{
\code{\link{rmd2html}}, \code{\link{rmd2pdf}}, \code{\link{rmd2docx}}, \code{\link{rmd2beamer}}

}

