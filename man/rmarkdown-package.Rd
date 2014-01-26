\docType{package}
\name{rmarkdown-package}
\alias{rmarkdown}
\alias{rmarkdown-package}
\title{R Markdown Document Conversion}
\description{
 Convert R Markdown documents into a variety of formats
 including HTML, MS Word, PDF, and Beamer.
}

\details{

The \pkg{rmarkdown} package includes high level functions for converting to a variety of formats. For example:

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

