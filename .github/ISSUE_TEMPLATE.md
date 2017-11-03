For general questions, please ask them on StackOverflow first, using the tags `r` and `rmarkdown` (other possibly relevant tags include `knitr`, `html`, `css`, and `latex`): http://stackoverflow.com/questions/ask Please come back here only if nobody answers your question there, and let us know the URL of your StackOverflow post. It is a better idea to use the wisdom of the community first.

For bug reports, please provide a minimal, self-contained, and reproducible example by reducing your example as much as possible right before the problem goes away. By doing this, you may be able to figure out what the problem really is before reporting to us. You can attach your example as a zip file here along with `devtools::session_info('rmarkdown')`, and screenshots are often very helpful to illustrate your issues.

Please do install and test the development version here before you report a bug:

```r
devtools::install_github("rstudio/rmarkdown")
```

To include a verbatim chunk of arbitrary text, wrap it in a pair of three backticks. When any line of your text contains N backticks (N >= 3), use N + 1 backticks to wrap the text, e.g. use four backticks to wrap three:

````
---
title: Hello World
output: html_document
---

A sample document.

```{r}
1 + 1  # a line of code
```

Another paragraph.
````

If it is just a chunk of R code (or other languages) and you want syntax highlighting, you may use three backticks to format it, e.g.

```r
rnorm(10)
```
