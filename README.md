### Overview

The **rmarkdown** package is a next generation implementation of R Markdown based on [pandoc](http://johnmacfarlane.net/pandoc/). This implementation brings many enhancements to R Markdown, including:

* Create HTML, PDF, and MS Word documents as well as [Beamer](https://bitbucket.org/rivanvx/beamer/wiki/Home) [ioslides](https://code.google.com/p/io-2012-slides/) and [reveal.js](http://lab.hakim.se/reveal-js/#/) presentations.
* New markdown syntax including expanded support for tables, definition lists, and bibliographies.
* A responsive (multi-device friendly) and themeable HTML template based on [Bootstrap 3](http://getbootstrap.com).
* Hooks for customizing HTML and PDF output (include CSS, headers, and footers).
* Include raw LaTeX within markdown for advanced customization of PDF output.
* Extensibility: easily define new formats for custom publishing requirements.

Note that PDF output (including Beamer slides) requires an installation of TeX. On Windows, the [MiKTeX](http://miktex.org/) distribution should be used rather than TeX Live.

See the [R Markdown documentation](http://rmarkdown.rstudio.com/) for full details.

### Installation

If you are working within RStudio then you can simply install the current [preview release](http://www.rstudio.com/ide/download/preview) of RStudio (both the rmarkdown package as well as pandoc are included).

If you want to use the rmarkdown package outside of RStudio then you can install the package as follows:

```
install.packages("devtools")
devtools::install_github("rstudio/rmarkdown")
```

A recent version of pandoc (>= 1.12.3) is also required. See the [pandoc installation instructions](PANDOC.md) for details on installing pandoc for your platform.

### Usage

The `render` function is used to convert R Markdown (Rmd) files into various output formats (the default is HTML). Calling `render` will knit the specified input document and then produce the final output document using pandoc:

```
render("input.Rmd")
```

You can also specify a plain markdown file in which case knitting will be bypassed:

```
render("input.md")
```

#### Output Formats

R Markdown documents can contain a metadata section that includes both title, author, and date information as well as options for customizing output. For example, this metadata included at the top of an Rmd file adds a table of contents and chooses a different HTML theme:

```
---
title: "Sample Document"
output:
  html_document:
    toc: true
    theme: united
---
```

R Markdown has built in support for several output formats (HTML, PDF, and MS Word documents as well as Beamer presentations). These formats can also be specified in metadata, for example:

```
---
title: "Sample Document"
output:
  pdf_document:
    toc: true
    highlight: zenburn
---
```

If you aren't specifying format options you can also just use a simple format name:

```
---
title: "Sample Document"
output: pdf_document
---
```

Multiple formats can be specified in metadata:

```
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

```
render("input.Rmd", "pdf_document")
```

If no explicit format name is passed to `render` then the first one defined will be used.

You can also render all format defined in the file with:

```S
render("input.Rmd", "all")
```

#### Shared Output Formats

You can also define output formats externally in a file named `output.yaml` located in the same directory as the R Markdown source file. For example:

```
html_document:
  toc: true
  theme: united
pdf_document:
  toc: true
  highlight: zenburn
```

Using an `output.yaml` file is a good way to share output settings across multiple R Markdown files in the same directory.

#### Output Format Functions

Output formats need not be specified in metadata. In fact, metadata is just a convenient way to invoke functions that implement output formats. There are five built-in output formats each exported as a function from the package:

- `html_document`
- `pdf_document`
- `word_document`
- `md_document`
- `beamer_presentation`
- `ioslides_presentation`
- `revealjs_presentation`

As you'd expect, these functions can also be invoked as part of the call to `render`, for example:

```
render("input.Rmd", html_document(toc = TRUE))
render("input.Rmd", pdf_document(latex_engine = "lualatex"))
render("input.Rmd", beamer_presentation(incremental = TRUE))
```

For more details on the options available for each format see their respective help topics.

### License

The **rmarkdown** package is licensed under the GPLv3 (http://www.gnu.org/licenses/gpl.html).






