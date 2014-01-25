
### Overview

The `rmarkdown` package is a next generation implementation of R Markdown based on [pandoc](http://johnmacfarlane.net/pandoc/). The package has several purposes:

1. Generate the most common types of output (HTML, MS Word, PDF, and Beamer) using a simple R function call with one argument.

2. At the same time, ensure that the R interface fully exposes the underlying options and capabilities of pandoc.

3. Provide a toolkit for writing R functions that support other flavors of pandoc output.

### Installation

To install the `rmarkdown` package and it's dependencies:

```
devtools::install_github(c("pandoc", "rmarkdown"), "rstudio")
```

A recent version of pandoc (>= 1.12.3) is also required. You can download pandoc from the [pandoc installation](http://johnmacfarlane.net/pandoc/installing.html) page. Alternatively, pandoc v1.12.3 is also included with recent [daily builds](http://www.rstudio.org/download/daily) of RStudio.







