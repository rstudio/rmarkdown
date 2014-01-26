
### Overview

The **rmarkdown** package is a next generation implementation of R Markdown based on [pandoc](http://johnmacfarlane.net/pandoc/). This implementation brings many enhancements to R Markdown, including:

* A wide variety of output formats including HTML, PDF, MS Word, and Beamer.
* New markdown syntax including expanded support for tables, definition lists, and citations.
* A responsive (multi-device friendly) HTML template based on [Bootstrap 3](http://getbootstrap.com).
* The ability to include raw LaTeX and LaTeX macros within markdown for advanced customization of PDF output.
* Extensive hooks for customizing HTML and PDF output.

### Installation

To install the package and it's dependencies:

```r
devtools::install_github(c("pandoc", "rmarkdown"), "rstudio")
```

A recent version of pandoc (>= 1.12.3) is also required. You can download pandoc from the [pandoc installation](http://johnmacfarlane.net/pandoc/installing.html) page. Alternatively, pandoc v1.12.3 is also included with recent [daily builds](http://www.rstudio.org/download/daily) of RStudio.

### Usage

The following functions will knit the specified input document and then produce the final output document using pandoc:

```r
rmd2html("input.Rmd")
rmd2pdf("input.Rmd")
```

You can also specify a plain markdown file in which case knitting will be bypassed:

```r
rmd2html("input.md")
```

All of the output formats have a corresponding options function which can be used to customize the ouptut, for example:

```r
rmd2html("input.Rmd", htmlOptions(toc = TRUE))
rmd2pdf("input.Rmd", pdfOptions(latex.engine = "lualatex"))
rmd2beamer("input.Rmd", beamerOptions(incremental = TRUE))
```

You can include custom CSS in HTML output using the `css` option. Combining this with setting the HTML `theme` to `NULL` provides for full control over all styles:

```r
rmd2html("input.Rmd", htmlOptions(css = "styles.css"))
rmd2html("input.Rmd", htmlOptions(theme = NULL, css = "styles.css"))
```

You can add custom content to HTML and PDF output using the `includes` option. For example:

```r
rmd2pdf("input.Rmd", pdfOptions(includes = includeOptions(before.body = "header.tex",
                                                          after.body = "footer.tex")))
```

You can also include arbitrary pandoc command line arguments in the call to the options function:

```r
rmd2pdf("input.Rmd", pdfOptions(toc = TRUE, "--listings"))
```

### License

The **rmarkdown** package is licensed under the GPLv2 (http://www.gnu.org/licenses/gpl-2.0.html).






