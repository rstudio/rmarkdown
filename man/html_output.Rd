\name{html_output}
\alias{html_output}
\alias{knit_print.html_output}
\title{Create HTML output}
\usage{
html_output(html, dependencies)

knit_print.html_output(x)
}
\arguments{
  \item{html}{HTML output}

  \item{dependencies}{List of HTML output dependencies
  (creating using \code{\link{html_dependency}})}

  \item{x}{Object to print as HTML}
}
\value{
Object of class \code{html_output} that when printed will
render in a browser (either in RStudio or extenally) and
when printed from within an R Markdown document will render
inline.
}
\description{
Create HTML output for display within an R Markdown
document, the RStudio Viewer pane, or an external web
browser.
}
\details{
See the documentation on
\href{http://rmarkdown.rstudio.com/developer_custom_html_output.html}{R
Markdown Custom HTML Output} for examples and additional
details.
}

