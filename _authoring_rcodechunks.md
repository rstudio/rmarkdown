---
title: "R Code Chunks"
output:
  html_document:
    toc_float: false
---

## Basic Usage

R code chunks can be used as a means render R output into documents or to simply display code for illustration. Here is a simple R code chunk that will result in both the code and it's output being included:

<pre class="markdown"><code>&#96;&#96;&#96;{r}
summary(cars)
&#96;&#96;&#96;
</code></pre>

To display the output of a code chunk but not the underlying R code, you specify the `echo=FALSE` option:

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo=FALSE}
summary(cars)
&#96;&#96;&#96;
</code></pre>

Note that R code chunks can also be used to render plots. To display a plot while omitting the code used to generate the plot you'd do this:

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo=FALSE}
plot(cars)
&#96;&#96;&#96;
</code></pre>

To display R code without evaluating it, you specify the `eval=FALSE` chunk option:

<pre class="markdown"><code>&#96;&#96;&#96;{r, eval=FALSE}
summary(cars)
&#96;&#96;&#96;
</code></pre>

## Table Output

By default data frames and matrixes are output as they would be in the R terminal (in a monospaced font). However, if you prefer that data be displayed with additional formatting you can use the `knitr::kable` function. For example:

<pre class="markdown"><code>&#96;&#96;&#96;{r, results='asis'}
knitr::kable(mtcars)
&#96;&#96;&#96;
</code></pre>

Note the use of the `results='asis'` chunk option. This is required to ensure that the raw table output isn't processed furthur by knitr. The `kable` function includes several options to control the maximum number of digits for numeric columns, alignment, etc (refer to the knitr package documentation for additional details).

## Caching

If document rendering becomes time consuming due to long computations or plots that are expensive to generate you can use knitr caching to improve performance. The documentation [knitr chunk and package options](http://yihui.name/knitr/options) describe how caching works and the [cache examples](http://yihui.name/knitr/demo/cache/) provide additional details.

If you want to enable caching globally for a document you can include a code chunk like this at the top of the document:

<pre class="markdown"><code>&#96;&#96;&#96;{r setup, include=FALSE}
knitr::opts_chunk$set(cache=TRUE)
&#96;&#96;&#96;
</code></pre>

If you run into problems with cached output you can always clear the knitr cache by removing the folder named with a `_cache` suffix within your document's directory.

## Rcpp Code Chunks

You can also create code chunks that define functions in C++ using [Rcpp Attributes](https://cran.rstudio.com/web/packages/Rcpp/vignettes/Rcpp-attributes.pdf). This is accomplished using the `engine = 'Rcpp'` chunk option. For example:

<pre class="cpp"><code>&#96;&#96;&#96;{r engine='Rcpp'}
#include &lt;Rcpp.h&gt;

// [[Rcpp::export]]
int fibonacci(const int x) {
    if (x == 0 || x == 1) return(x);
    return (fibonacci(x - 1)) + fibonacci(x - 2);
}
&#96;&#96;&#96;
</code></pre>

Because `fibonacci` was defined with the `Rcpp::export` attribute it can now be called as a normal R function:

<pre class="markdown"><code>&#96;&#96;&#96;{r}
fibonacci(10L)
fibonacci(20L)
&#96;&#96;&#96;
</code></pre>

Note that caching should not be used with Rcpp code chunks (since the compiled C++ 
function will not survive past the lifetime of the current R session).

## Learning More

The knitr package is an extremely powerful tool for dynamic content generation and is worth studying in detail to understand all of it's features and capabilities. Good resources for learning more about knitr include:

* The [knitr website](http://yihui.name/knitr/)
* The [knitr manual](http://bit.ly/117OLVl) and [knitr graphics manual](http://bit.ly/114GNdP)
* The galleries of [demos](http://yihui.name/knitr/demos) and [examples](https://github.com/yihui/knitr-examples)
* The book [Dynamic Documents with R and knitr](http://www.amazon.com/dp/1482203537/ref=cm_sw_su_dp) (written by the creator of knitr, [Yihui Xie](http://yihui.name))




