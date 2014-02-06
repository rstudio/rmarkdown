
### Overview

The **rmarkdown** package is a next generation implementation of R Markdown based on [pandoc](http://johnmacfarlane.net/pandoc/). This implementation brings many enhancements to R Markdown, including:

* Create HTML, PDF, and MS Word documents as well as [Beamer](https://bitbucket.org/rivanvx/beamer/wiki/Home) and [reveal.js](http://lab.hakim.se/reveal-js/#/) presentations
* New markdown syntax including expanded support for tables, definition lists, and bibliographies.
* A responsive (multi-device friendly) and themeable HTML template based on [Bootstrap 3](http://getbootstrap.com).
* Hooks for customizing HTML and PDF output (include CSS, headers, and footers).
* Include raw LaTeX within markdown for advanced customization of PDF output.
* Extensibility: easily define new formats for custom publishing requirements.

Note that creating PDF and Beamer output requires that LaTeX be installed.

### Installation

To install the package and it's dependencies:

```
devtools::install_github("rmarkdown", "rstudio")
```

A recent version of pandoc (>= 1.12.3) is also required. Recent [daily builds](http://www.rstudio.org/download/daily) of RStudio include pandoc v1.12.3. If you only intend to use the **rmarkdown** package within RStudio you can rely on this version.

If you are not running within RStudio then you can obtain pandoc as follows:

#### Windows and Mac OS X

The [pandoc installation](http://johnmacfarlane.net/pandoc/installing.html) page includes easy to use installers for Windows and Mac OS X.

#### Linux

The version of pandoc included in the standard repositories is not recent enough for use with the **rmarkdown** package. You can install a more recent version of pandoc by installing the Haskell Platform and then following these instructions for [building pandoc from source](http://johnmacfarlane.net/pandoc/installing.html#all-platforms).

This method installs a large number of Haskell dependencies so might not be desirable. You can also obtain a standalone version of pandoc without the dependencies as follows:

##### Older Systems (RedHat/CentOS 5 & 6)

For older Linux systems you can obtain a standalone version of pandoc v1.12.3 (with no Haskell dependencies) from http://petersen.fedorapeople.org/pandoc-standalone/ as follows:

```
$ sudo wget -P /etc/yum.repos.d/ http://petersen.fedorapeople.org/pandoc-standalone/pandoc-standalone.repo
$ yum install pandoc pandoc-citeproc
```

##### Newer Systems (Debian/Ubuntu/Fedora)

For newer Linux systems it's possible to build a standalone version of pandoc v1.12.3 using this script (developed and tested on Ubuntu 10.04):

[https://github.com/rstudio/rmarkdown/blob/master/tools/build-pandoc-standalone-debian.sh](https://github.com/rstudio/rmarkdown/blob/master/tools/build-pandoc-standalone-debian.sh)

This is the script RStudio uses to create it's bundled version of pandoc for Debian.

You can also make pandoc available to the system by soft-linking the binaries included with RStudio:

```
$ sudo ln -s /usr/lib/rstudio/bin/pandoc/pandoc /usr/local/bin
$ sudo ln -s /usr/lib/rstudio/bin/pandoc/pandoc-citeproc /usr/local/bin
```

If you are running RStudio Server the commands would be:

```
$ sudo ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc /usr/local/bin
$ sudo ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc-citeproc /usr/local/bin
```

If you aren't running RStudio at all you could also copy the binaries out of the RStudio `bin/pandoc` directory and locate them within `/usr/local/bin` on your target system.




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
- `revealjs_presentation`

As you'd expect, these functions can also be invoked as part of the call to `render`, for example:

```
render("input.Rmd", html_document(toc = TRUE))
render("input.Rmd", pdf_document(latex_engine = "lualatex"))
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






