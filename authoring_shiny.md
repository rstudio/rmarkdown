---
title: "Interactive Documents with Shiny"
---

## Overview

You can use the [Shiny](http://shiny.rstudio.com) web application framework to make your R Markdown documents fully interative. For example, readers of your document could change the assumptions underlying a data visualization and see the results immediately. 

Here's an example of a simple R Markdown document that contains an interactive plot:

![Shiny Hist Plot](images/shiny-interactive-plot.png)

Users can manipulate the number of bins and bandwidth adjustment and the plot automatically updates to reflect the changes. Adding an interactive plot to a document is straightfoward: simply wrap the code used to generate the plot in the `renderPlot` function and specify the user inputs (e.g. the `selectInput` and `sliderInput` in the code below).

For example, here's what the code used to generate the document above looks like:

![Shiny Code](images/shiny-code.png)

The resulting "Shiny Doc" combines the expressiveness of R Markdown with the interactivity of Shiny. These documents are run just like Shiny applications: they can be run locally or be deployed to [Shiny Server v1.2](http://www.rstudio.com/shiny/server/) or [ShinyApps](http://www.rstudio.com/shiny/hosted/) (see the [Deployment] section below for more details).

## Getting Started

### Prerequisites

Working with Shiny Docs requires an up to date version of the [RStudio Preview Release](http://www.rstudio.com/ide/download/preview) (v0.98.872 or later) so be sure to update RStudio before trying out these features. 

The [RStudio Preview Release](http://www.rstudio.com/ide/download/preview) includes everything you need to create Shiny documents (including the latest development version of the Shiny package). If you are not using RStudio you can also install the required version of Shiny as follows:

```r
devtools::install_github("rstudio/shiny")
```

### Creating a Shiny Doc

To create a new Shiny Doc open the **New R Markdown** dialog in RStudio and choose to create a document with the "Shiny Document" template:

![New R Markdown Shiny Document](images/new-shiny-document.png)

You can run a document locally using the **Run Document** command on the editor toolbar:

![Shiny Run Document](images/shiny-run-document.png)

You can also run the document from the console using the `rmarkdown::run` function:

```r
rmarkdown::run("MyShinyDocument.Rmd")
```

If you haven't used Shiny before some of the code will be unfamiliar to you (the basics are explained below). In addition, the [Shiny Tutorial](http://shiny.rstudio.com/tutorial) is a good place to learn more.

### Inputs and Outputs

You can embed Shiny inputs and outputs in your document. Outputs are automatically updated whenever inputs change. In this example we create a `numericInput` with the name "rows" and then refer to its value via `input$rows` when generating output:

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo = FALSE}
numericInput("rows", "How many cars?", 5)

renderTable({
  head(cars, input$rows)
})
&#96;&#96;&#96;
</code></pre>

![Shiny Cars Table](images/shiny-cars-table.gif)

In this example the output code was wrapped in a call to `renderTable`. There are many other render functions in Shiny that can be used for plots, printed R output, and more. This example uses `renderPlot` to create dynamic plot output:

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo = FALSE}
sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)

renderPlot({
  x <- faithful[, 2]  # Old Faithful Geyser data
  bins <- seq(min(x), max(x), length.out = input$bins + 1)

  # draw the histogram with the specified number of bins
  hist(x, breaks = bins, col = 'darkgray', border = 'white')
})
&#96;&#96;&#96;
</code></pre>

![Shiny Hist Plot](images/shiny-hist-plot.gif)

## Deployment

### Shiny Server

[Shiny Server v1.2](http://www.rstudio.com/shiny/server/) or higher supports deployment of Shiny Docs. To deploy one or more Shiny Docs you simply make them available within a directory hosted by Shiny Server and address them by filename. For example:

http://example.com/reports/analysis.Rmd 

Note that in order to serve Shiny Docs you need to install the latest versions of both the **rmarkdown** and **shiny** packages on your server as follows:

```r
devtools::install_github(c("rstudio/rmarkdown", "rstudio/shiny"))
```

### ShinyApps

You can publish Shiny Docs to the [ShinyApps](http://www.rstudio.com/shiny/hosted/) hosted service. To do this you should ensure that you have:

1. An account on ShinyApps (use the [signup form](http://www.shinyapps.io/signup.html) to request an account).

2. The very latest version of the **shinyapps** R package. You can install this as follows:

```r
devtools::install_github("rstudio/shinyapps")
```

You can then deploy a Shiny Doc the same way that you currently deploy Shiny apps. From the working directory containing the document(s) just execute:

```r
shinyapps::deployApp()
```

If you are using RStudio you can also use the **Deploy** button available when working with a Shiny Doc:

![Shinyapps Deploy](images/shinyapps-deploy.png)

## Learning More

This introduction just scratches the surface of the types of interactive documents you can create with R Markdown and Shiny. See the following resources to learn more:

1. [Embedded Shiny Apps](authoring_embedded_shiny.html) describes how you can embed entire Shiny applications inside an R Markdown document.

2. [Creating Shiny Widgets](authoring_shiny_widgets.html) covers creating re-usable Shiny widgets that enable others to embed interactive components in their documents with a single function call.

3. [Advanced Topics](authoring_shiny_advanced.html) includes details on converting existing R Markdown documents to Shiny Docs and creating multiple-page Shiny Docs.

4. Finally, the [Shiny Dev Center](http://shiny.rstudio.com) includes extensive articles, tutorials, and examples to help you learn more about Shiny.






