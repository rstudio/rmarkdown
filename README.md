
### Overview

The **rmarkdown** package is a next generation implementation of R Markdown based on [pandoc](http://johnmacfarlane.net/pandoc/). This implementation brings many enhancements to R Markdown, including:

* A wide variety of output formats including HTML, PDF, MS Word, and Beamer.
* New markdown syntax including expanded support for tables, definition lists, and citations.
* A responsive (multi-device friendly) HTML template based on [Bootstrap 3](http://getbootstrap.com).
* The ability to include raw LaTeX and LaTeX macros within markdown for advanced customization of PDF output.
* Extensive hooks for customizing HTML and PDF output.

The **rmarkdown** package has several goals:

1. Render attractive HTML, PDF, MS Word, and Beamer output using a simple R function call with one argument.

2. At the same time, ensure that the R interface fully exposes the underlying options and capabilities of pandoc.

3. Provide a toolkit for writing R functions that support other flavors of pandoc output.

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

You can also include arbitrary pandoc command line arguments in the call to the options function:

```r
rmd2pdf("input.Rmd", pdfOptions(toc = TRUE, "--listings"))
```

### Custom Formats

To create a custom output format you simply define a new function and delegate as necessary to the functions within the **rmarkdown** package. The basic form of an output function is:

1. Check whether a knit is required and if so set appropriate knitr options and hooks.

2. Create a character vector of pandoc options appropriate to the output you are generating.

3. Call the `rmd2pandoc` function to perform the knit and call pandoc with the specified options.

For example, here's the implementation of an output function that uses a custom HTML template:

```r
rmd2foo <- function(input,
                    output = NULL,
                    envir = parent.frame(),
                    quiet = FALSE,
                    encoding = getOption("encoding")) {

  # check for knit and define hooks for HTML output
  if (rmarkdown::knitRequired(input))
    rmarkdown::knitrRenderHtml("html", 6, 6)

  # base pandoc options (default html options plus table of contents)
  options <- rmarkdown::htmlOptions(toc = TRUE)

  # add a custom html template
  options <- c(options, pandoc::templateOptions(
                          system.file("templates/foo", package = "footools")))

  # perform the conversion
  rmarkdown::rmd2pandoc(input, "html", options, output, envir, quiet, encoding)
}
```

Note that this output function is defined within a package so it's template is accessed using `system.file`. For more information on custom templates see the documentation on [pandoc templates](http://johnmacfarlane.net/pandoc/demo/example9/templates.html).

If you wanted to provide some rendering options to users of the function you would add an `options` parameter as the second argument and define a function that generates the appropriate options (e.g. `fooOptions`). The predefined formats (e.g. `rmd2html`, `rmd2pdf`, etc.) all use this pattern.

Note that while it's possible to create a custom output function with a different signature (e.g. exposing options as arguments to the top level function or not exposing the `envir` and/or `quiet` parameters) this is not recommended. Conforming to the idiom defined by the built-in output functions ensures consistency for end-users and enables front end tools to generate calls (e.g. for document preview) to R Markdown output functions in a standard fashion.

### License

The **rmarkdown** package is licensed under the GPLv2 (http://www.gnu.org/licenses/gpl-2.0.html).






