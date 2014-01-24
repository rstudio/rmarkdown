\name{rmdMetadata}
\alias{rmdMetadata}

\title{R Markdown Metadata}

\description{Metadata can be used to specify the title, author, and date of a document; to generate bibliographies; and for some formats to modify the apperance of the generated output.
}

\section{Overview}{

Rmd files include a metadata section (typically located at the top of the file) that can specify (among other things) the title, author, and date of the document. Metadata is adheres to the \href{http://www.yaml.org/}{YAML} format and is delimted by lines containing three dashes (\code{---}). Here is an example metadata section:

\preformatted{---
title: "Crop Analysis Q3 2013"
author: Martha Smith
date: October 23rd, 2013
---
}

Note that the \code{title} field is quoted. This is because titles often contained embedded colons (\code{:}) and colons followed by a space need to be quoted in YAML.

\subsection{Title Sections}{
When title, author, and date metadata is provided it's used to automatically create a title section within output documents. If you don't want this section included in your document then you should remove the corresponding metadata fields.
}
}

\section{Citation Metatdata}{

R Markdown documents can also include footnotes and citations, with support for a wide variety of bibliography formats and output styles.

To define the bibliography and citation styles for a document you include a bibliography and citation style (csl) file in the same directory as your R Markdown document and then add corresponding fields to the document's metadata. For example:

\preformatted{---
title: "Crop Analysis Q3 2013"
bibliography: crop-analysis.bib
csl: chicago-author-date.csl
---
}

You can find more information on the markdown syntax for citations within the \href{http://johnmacfarlane.net/pandoc/README.html}{pandoc documentation}.
}

\section{Apperance Metadata}{

When generating PDF and Beamer output there are also a number of other metadata fields that can be included to customize the appearance and theme of PDF output. For more details see the documentation for \code{\link{rmd2pdf}} and \code{\link{rmd2beamer}}.

}








