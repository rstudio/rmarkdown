\name{rmdCitations}
\alias{rmdCitations}

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

You can find more information on the markdown syntax for citations within the pandoc documentation on  \href{http://johnmacfarlane.net/pandoc/demo/example19/Citations.html}{citations} and \href{http://johnmacfarlane.net/pandoc/demo/example19/Footnotes.html}{footnotes}.
}

