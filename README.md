
### Overview

The **rmarkdown** package is a next generation implementation of R Markdown based on [pandoc](http://johnmacfarlane.net/pandoc/). This implementation brings many enhancements to R Markdown, including:

* A wide variety of output formats including HTML, PDF, MS Word, and Beamer.
* New markdown syntax including expanded support for tables, definition lists, and citations.
* A responsive (multi-device friendly) HTML template based on [Bootstrap 3](http://getbootstrap.com).
* The ability to include raw LaTeX within markdown for advanced customization of PDF output.
* Extensive hooks for customizing HTML and PDF output.

Note that creating PDF and Beamer output requires that LaTeX be installed.

### Installation

To install the package and it's dependencies:

```
devtools::install_github(c("pandoc", "rmarkdown"), "rstudio")
```

#### Pandoc

A recent version of pandoc (>= 1.12.3) is also required. There are a few ways to obtain an up to date version of pandoc:

1. If you running within RStudio you can rely on the version of pandoc that is included with recent [daily builds](http://www.rstudio.org/download/daily) of RStudio.

2. Follow the instructions for your platform on the [pandoc installation](http://johnmacfarlane.net/pandoc/installing.html) page.

3. If you are running on Linux without RStudio and don't want to install the Haskell dependencies required for pandoc you can also copy the `pandoc` and `pandoc-citeproc` binaries out of the RStudio `bin/pandoc` directory and place them somewhere on your system path.


### Usage

The `render` function is used to convert R Markdown (Rmd) files into various output formats (the default is HTML). Calling `render` will knit the specified input document and then produce the final output document using pandoc:

```
render("input.Rmd")
```

You can also specify a plain markdown file in which case knitting will be bypassed:

```
render("input.md")
```

R Markdown documents can now contain a metadata section that includes both title, author, and date information as well as options for customizing output. For example, this metadata included at the top of an Rmd file adds a table of contents and chooses a different HTML theme:

```
---
title: "Sample Document"
author: John Smith
output:
  format: html_document
  toc: true
  theme: united
---
```

R Markdown has built in support for several output formats (HTML, PDF, and MS Word documents as well as Beamer presentations). These formats can also be specified in metadata, for example:

```
---
title: "Sample Document"
author: John Smith
output:
  format: pdf_document
  toc: true
---
```

Output formats need not be defined in metadata. They can also be specified programatically from R, for example:

```
render("input.Rmd", html_document(toc = TRUE))
render("input.Rmd", pdf_document(latex.engine = "lualatex"))
render("input.Rmd", beamer_presentation(incremental = TRUE))
```

You can include custom CSS in HTML output using the `css` option. Combining this with setting the HTML `theme` to `NULL` provides for full control over all styles:

```
render("input.Rmd", html_document(css = "styles.css"))
render("input.Rmd", html_document(theme = NULL, css = "styles.css"))
```

You can add custom content to HTML and PDF output using the `includes` option. For example:

```
includes <- pandoc::include_options(before.body = "header.tex",
                                    after.body = "footer.tex"))

render("input.Rmd", pdf_document(includes = includes))
```

You can also include arbitrary pandoc command line arguments:

```
render("input.Rmd", pdf_document(toc = TRUE, pandoc.args = c("--listings")))
```

### Custom Formats

You aren't limited to the built in output formats like `html_document` and `pdf_document`. You can also create custom output formats that utilize arbitrary knitr options, knitr hooks, and pandoc command line options.

To create a custom format you write a function that returns an object of class "rmarkdown_output_format". The easiest way to do this is to call the `rmarkdown::output_format` function. For example, here's a very simple custom format that converts from R Markdown to HTML using the default pandoc HTML template:

```
custom_format <- function() {

  knitr <- knitr_options(
    opts_chunk = list(dev = 'png', fig.width = 7, fig.height = 5)
  )

  pandoc <- pandoc_options(
    to = "html",
    args = c("--smart", "--standalone")
  )

  rmarkdown::output_format(knitr = knitr, pandoc = pandoc)
}
```

This custom format has no parameters however in practice you'll often want to provide options as function parameters to allow callers to customize the behavior of the format.

### License

The **rmarkdown** package is licensed under the GPLv3 (http://www.gnu.org/licenses/gpl.html).






