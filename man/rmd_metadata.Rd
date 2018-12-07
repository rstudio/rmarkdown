\name{rmd_metadata}
\alias{rmd_metadata}

\title{R Markdown Metadata}

\description{

Rmd files include a metadata section (typically located at the top of the file) that can specify (among other things) the title, author, and date of the document. Metadata adheres to the \href{https://yaml.org}{YAML} format and is delimited by lines containing three dashes (\code{---}). Here is an example metadata section:

\preformatted{---
title: "Crop Analysis Q3 2013"
author: Martha Smith
date: October 23rd, 2013
---
}

Note that the \code{title} field is quoted. This is because titles often contained embedded colons (\code{:}) and colons followed by a space need to be quoted in YAML.
}

\details{
When title, author, and date metadata is provided it's used to automatically create a title section within output documents. If you don't want this section included in your document then you should remove the corresponding metadata fields.

When generating PDF and Beamer output there are also a number of other metadata fields that can be included to customize the appearance and theme of PDF output. For more details see the documentation for \code{\link{pdf_document}} and \code{\link{beamer_presentation}}.

}







