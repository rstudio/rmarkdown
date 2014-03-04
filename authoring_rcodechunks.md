---
title: "R Code Chunks"
---

## Basic Usage

R code chunks can be used as a means render R output into documents or to simply display code for illustration. Here is a simple R code chunk that will result in both the code and it's output being included:

    ```{r}
    summary(cars)
    ```

To display the output of a code chunk but not the underlying R code, you specify the `echo=FALSE` option:

    ```{r, echo=FALSE}
    summary(cars)
    ```

Note that R code chunks can also be used to render plots. To display a plot while omitting the code used to generate the plot you'd do this:

    ```{r, echo=FALSE}
    plot(cars)
    ```
To display R code without evaluating it, you specify the `eval=FALSE` chunk option:

    ```{r, eval=FALSE}
    summary(cars)
    ```

## Caching

If document rendering becomes time consuming due to long computations or plots that are expensive to generate you can use knitr caching to improve performance. The documentation [knitr chunk and package options](http://yihui.name/knitr/options) describe how caching works and the [cache examples](http://yihui.name/knitr/demo/cache/) provide additional details.

If you want to enable caching globally for a document you can include a code chunk like this at the top of the document:

    ```{r setup, include=FALSE}
    opts_chunk$set(cache=TRUE)
    ```

If you run into problems with cached output you can always clear the knitr cache by removing the folder named with a `_cache` suffix within your document's directory.

## Learning More

The knitr package is an extremely powerful tool for dynamic content generation and is worth studying in detail to understand all of it's features and capabilities. Good resources for learning more about knitr include:

* The [knitr website](http://yihui.name/knitr/)
* The [knitr manual](http://bit.ly/117OLVl) and [knitr graphics manual](http://bit.ly/114GNdP)
* The galleries of [demos](http://yihui.name/knitr/demos) and [examples](https://github.com/yihui/knitr-examples)
* The book [Dynamic Documents with R and knitr](http://www.amazon.com/dp/1482203537/ref=cm_sw_su_dp) (written by the creator of knitr, [Yihui Xie](http://yihui.name))




