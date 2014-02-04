
### Overview

The **rmarkdown** package is a next generation implementation of R Markdown based on [pandoc](http://johnmacfarlane.net/pandoc/). This implementation brings many enhancements to R Markdown, including:

* A wide variety of built-in output formats including HTML, PDF, MS Word, and Beamer.
* New markdown syntax including expanded support for tables, definition lists, and citations.
* A responsive (multi-device friendly) HTML template based on [Bootstrap 3](http://getbootstrap.com).
* Hooks for customizing HTML and PDF output (include css, headers, and footers).
* The ability to include raw LaTeX within markdown for advanced customization of PDF output.
* Extensibility: easily define new formats for custom publishing requirements.

Note that creating PDF and Beamer output requires that LaTeX be installed.

### Installation

To install the package and it's dependencies:

```
devtools::install_github("rmarkdown", "rstudio")
```

#### Pandoc

A recent version of pandoc (>= 1.12.3) is also required. A few ways to obtain an up to date version of pandoc are described below.

##### RStudio

Recent [daily builds](http://www.rstudio.org/download/daily) of RStudio include pandoc v1.12.3. If you only intend to use the **rmarkdown** package within RStudio you can rely on this version.

##### Pandoc website

If you want to use R Markdown outside of RStudio you should obtain a copy of pandoc from the [pandoc installation](http://johnmacfarlane.net/pandoc/installing.html) page. Easy to use installers for standalone versions of pandoc are available for Windows and Mac OS X.

For Linux systems, you may need to install a large number of Haskell dependencies to get pandoc installed. If you are running on Linux outside of RStudio and don't want to install these dependencies see below.

##### RedHat/CentOS 5 & 6

For RedHat based systems you can obtain a standalone version of pandoc v1.12.3 (with no Haskell dependencies) from http://petersen.fedorapeople.org/pandoc-standalone/ as follows:

```
$ sudo wget -P /etc/yum.repos.d/ http://petersen.fedorapeople.org/pandoc-standalone/pandoc-standalone.repo
$ yum install pandoc pandoc-citeproc
```

##### Debian/Ubuntu

For Debian based systems you can obtain a standalone version of pandoc v1.12.3 by copying the binaries out of the RStudio distribution as follows:

```
$ sudo cp /usr/lib/rstudio/bin/pandoc/pandoc* /usr/bin/
```


### Usage

The `render` function is used to convert R Markdown (Rmd) files into various output formats (the default is HTML). Calling `render` will knit the specified input document and then produce the final output document using pandoc:

```
render("input.Rmd")
```

You can also specify a plain markdown file in which case knitting will be bypassed:

```
render("input.md")
```

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

If you aren't specifing format options you can also just use a simple format name:

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

Output formats need not be specified in metadata. In fact, metadata is just a convenient way to invoke functions that implement output formats. There are five built-in output formats each exported as a function from the package:

- `html_document`
- `pdf_document`
- `word_document`
- `md_document`
- `beamer_presentation`

As you'd expect, these functions can also be invoked as part of the call to `render`, for example:

```
render("input.Rmd", html_document(toc = TRUE))
render("input.Rmd", pdf_document(latex.engine = "lualatex"))
render("input.Rmd", beamer_presentation(incremental = TRUE))
```

For more details on the options available for each format see their respective help topics.

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

  rmarkdown::output_format(knitr, pandoc)
}
```

Once you've created a custom format it can be used in the exact same fashion as the built-in formats. For example, assuming the format above was defined in a package named `pubtools` could you could specify it in Rmd metadata as follows:

```
---
title: "Sample Document"
output: pubtools::custom_format
---
```

Alternatively you could use it in a call to `render`:

```
render("input.Rmd", pubtools::custom_format())
```

This custom format has no parameters however in practice you'll often want to provide options as function parameters to allow callers to customize the behavior of the format.


### License

The **rmarkdown** package is licensed under the GPLv3 (http://www.gnu.org/licenses/gpl.html).






