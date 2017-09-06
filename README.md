### Overview

[![Build Status](https://travis-ci.org/rstudio/rmarkdown.svg?branch=master)](https://travis-ci.org/rstudio/rmarkdown)

The **rmarkdown** package is a next generation implementation of R Markdown based on [pandoc](http://johnmacfarlane.net/pandoc/). This implementation brings many enhancements to R Markdown, including:

-   Create HTML, PDF, and MS Word documents as well as [Beamer](https://bitbucket.org/rivanvx/beamer/wiki/Home), [ioslides](https://code.google.com/p/io-2012-slides/), and [Slidy](http://www.w3.org/Talks/Tools/Slidy2/) presentations.
-   New markdown syntax including expanded support for tables, definition lists, and bibliographies.
-   Hooks for customizing HTML and PDF output (include CSS, headers, and footers).
-   Include raw LaTeX within markdown for advanced customization of PDF output.
-   Compile HTML, PDF, or MS Word notebooks from R scripts.
-   Extensibility: easily define new formats for custom publishing requirements.
-   Create interactive R Markdown documents using Shiny.

Note that PDF output (including Beamer slides) requires an installation of TeX.

See the [R Markdown documentation](http://rmarkdown.rstudio.com/) for full details.

### Installation

If you are working within RStudio then you can simply install the [current release](http://www.rstudio.com/ide/download/preview) of RStudio (both the rmarkdown package and pandoc are included).

If you want to use the rmarkdown package outside of RStudio then you can install the package from CRAN as follows:

```r
install.packages("rmarkdown")
```

A recent version of pandoc (&gt;= 1.12.3) is also required. See the [pandoc installation instructions](PANDOC.md) for details on installing pandoc for your platform.

### Usage

The `render` function is used to convert R Markdown (Rmd) files into various output formats (the default is HTML). Calling `render` will knit the specified input document and then produce the final output document using pandoc:

```r
render("input.Rmd")
```

You can also specify a plain markdown file in which case knitting will be bypassed:

```r
render("input.md")
```

#### Output Formats

R Markdown documents can contain a metadata section that includes both title, author, and date information as well as options for customizing output. For example, this metadata included at the top of an Rmd file adds a table of contents and chooses a different HTML theme:

```yaml
---
title: "Sample Document"
output:
  html_document:
    toc: true
    theme: united
---
```

R Markdown has built in support for several output formats (HTML, PDF, and MS Word documents as well as Beamer presentations). These formats can also be specified in metadata, for example:

```yaml
---
title: "Sample Document"
output:
  pdf_document:
    toc: true
    highlight: zenburn
---
```

If you aren't specifying format options you can also just use a simple format name:

```yaml
---
title: "Sample Document"
output: pdf_document
---
```

Multiple formats can be specified in metadata:

```yaml
---
title: "Sample Document"
output:
  html_document:
    toc: true
    theme: united
  pdf_document:
    toc: true
    highlight: zenburn
---
```

To select from the various formats defined you can pass a format name to `render`. For example:

```r
render("input.Rmd", "pdf_document")
```

If no explicit format name is passed to `render` then the first one defined will be used. You can also render all formats defined in the file with:

```r
render("input.Rmd", "all")
```

#### Shared Output Formats

You can also define output formats externally in a file named `_output.yml` located in the same directory as the R Markdown source file. For example:

```yaml
html_document:
  toc: true
  theme: united
pdf_document:
  toc: true
  highlight: zenburn
```

Using an `_output.yml` file is a good way to share output settings across multiple R Markdown files in the same directory.

#### Output Format Functions

Output formats need not be specified in metadata. In fact, metadata is just a convenient way to invoke functions that implement output formats. There are seven built-in output formats each exported as a function from the package:

-   `html_document`
-   `pdf_document`
-   `word_document`
-   `md_document`
-   `beamer_presentation`
-   `ioslides_presentation`
-   `slidy_presentation`

As you'd expect, these functions can also be invoked as part of the call to `render`, for example:

```r
render("input.Rmd", html_document(toc = TRUE))
render("input.Rmd", pdf_document(latex_engine = "lualatex"))
render("input.Rmd", beamer_presentation(incremental = TRUE))
```

For more details on the options available for each format see their respective help topics.

### License

The **rmarkdown** package is licensed under the GPLv3 (<http://www.gnu.org/licenses/gpl.html>).
