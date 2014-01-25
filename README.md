
### Overview

The **rmarkdown** package is a next generation implementation of R Markdown based on [pandoc](http://johnmacfarlane.net/pandoc/). This implementation brings many enhancements to R Markdown, including:

* A wide variety of output formats including HTML, PDF, MS Word, and Beamer,
* Highly extensible templates for customizing output.
* New markdown syntax including expanded support for tables, definition lists, and citations.
* For advanced customization of PDF output, the ability to include raw LaTeX and LaTeX macros within markdown.

The **rmarkdown** package has several goals:

1. Generate attractive HTML, PDF, MS Word, and Beamer output using a simple R function call with one argument.

2. At the same time, ensure that the R interface fully exposes the underlying options and capabilities of pandoc.

3. Provide a toolkit for writing R functions that support other flavors of pandoc output.

### Installation

To install the package and it's dependencies:

```
devtools::install_github(c("pandoc", "rmarkdown"), "rstudio")
```

A recent version of pandoc (>= 1.12.3) is also required. You can download pandoc from the [pandoc installation](http://johnmacfarlane.net/pandoc/installing.html) page. Alternatively, pandoc v1.12.3 is also included with recent [daily builds](http://www.rstudio.org/download/daily) of RStudio.

### License

The **rmarkdown** package is licensed under the GPLv2 (http://www.gnu.org/licenses/gpl-2.0.html).






