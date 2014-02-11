\name{rmd_citations}
\alias{rmd_citations}

\title{R Markdown Citations}

\description{R Markdown documents can also include footnotes and citations, with support for a wide variety of bibliography formats and output styles.
}

\details{

To define the bibliography and citation styles for a document you include a bibliography and citation style (csl) file in the same directory as your R Markdown document and then add corresponding fields to the document's metadata. For example:

\preformatted{---
title: "Crop Analysis Q3 2013"
bibliography: crop-analysis.bib
csl: chicago-author-date.csl
---
}

For \code{\link{pdf_document}} and \code{\link{beamer_presentation}} you can also use \code{bibtex} or \code{natbib} for bibliographies. In this case you can also include the following fields in metadata:

\describe{
\item{\code{biblio-style}}{LaTeX bibliography style (used with \code{natbib} option)}
\item{\code{biblio-files}}{Bibliography files to use in LaTeX (used with \code{natbib} or \code{biblatex} options)}
}

You can find more information on the markdown syntax for citations within the pandoc documentation on  \href{http://johnmacfarlane.net/pandoc/demo/example19/Citations.html}{citations} and \href{http://johnmacfarlane.net/pandoc/demo/example19/Footnotes.html}{footnotes}.
}

