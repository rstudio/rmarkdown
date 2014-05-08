---
title: "Interactive Documents with Shiny"
output: 
  html_document:
    toc: true
    toc_depth: 3
---

## Overview

You can use the [Shiny](http://shiny.rstudio.com) web application framework to make your documents fully interative. For example, readers of your document could change the assumptions underlying a data visualization and see the results immediately. 

This demonstrates how a standard R plot can be made interactive by wrapping it in the Shiny `renderPlot` function. The `selectInput` and `sliderInput` functions create the input widgets used to drive the plot.

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo = FALSE}
inputPanel(
  selectInput("n_breaks", label = "Number of bins:",
              choices = c(10, 20, 35, 50), selected = 20),
  
  sliderInput("bw_adjust", label = "Bandwidth adjustment:",
              min = 0.2, max = 2, value = 1, step = 0.2)
)

renderPlot({
  hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$n_breaks),
       xlab = "Duration (minutes)", main = "Geyser eruption duration")
  
  dens <- density(faithful$eruptions, adjust = input$bw_adjust)
  lines(dens, col = "blue")
})
&#96;&#96;&#96;
</code></pre>

This is what the interactive plot would look like rendered within the document:

![Shiny Hist Plot](images/shiny-interactive-plot.png)

The resulting "Shiny Doc"" combines the expressiveness of R Markdown with the interactivity of Shiny. Note that Shiny Docs can currently only be run locally on the desktop. Support for publishing to Shiny Server will be available soon.

## Getting Started

### Prerequisites

Working with Shiny Docs requires an up to date version of the [RStudio Preview Release](http://www.rstudio.com/ide/download/preview) (v0.98.829 or later) so be sure to update RStudio before trying out these features. 

You'll also need the development versions of both the **knitr** and **shiny** packages, which you can install as follows:

```r
devtools::install_github(c("yihui/knitr", "rstudio/shiny"))
```

### Creating a Shiny Doc

To create a new Shiny Doc open the **New R Markdown** dialog in RStudio and choose to create a document with the "Shiny Document" template:

![New R Markdown Shiny Document](images/new-shiny-document.png)

Note that if you haven't installed up to date versions of RStudio, knitr, and shiny as detailed above then the "Shiny" templates won't appear in the list.

You can run a document locally using the **Run Document** command on the editor toolbar:

![Shiny Run Document](images/shiny-run-document.png)

You can also run the document from the console using the `rmarkdown::run` function:

```r
rmarkdown::run("MyShinyDocument.Rmd")
```

If you haven't used Shiny before some of the code will be unfamiliar to you. The [Shiny Tutorial](http://shiny.rstudio.com/tutorial) is a good starting point for learning more.

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


## Embedded Shiny Apps

It's also possible to embed an entire Shiny application within a document. There are two syntaxes for this: 

1) Defining the application inline using the `shinyApp` function; or

2) Referring to an external application directory using the `shinyAppDir` function.

### Inline Applications

This example uses an inline definition:

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo = FALSE}
shinyApp(
  
  ui = fluidPage(
    selectInput("region", "Region:", 
                choices = colnames(WorldPhones)),
    plotOutput("phonePlot")
  ),
  
  server = function(input, output) {
    output$phonePlot <- renderPlot({
      barplot(WorldPhones[,input$region]*1000, 
              ylab = "Number of Telephones", xlab = "Year")
    })
  },
  
  options = list(height = 500)
)
&#96;&#96;&#96;
</code></pre>

Note the use of the `height` parameter to determine how much vertical space the embedded application should occupy.

### External Applications

This example embeds a Shiny application defined in another directory:

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo = FALSE}
shinyAppDir(
  system.file("examples/06_tabsets", package="shiny"),
  options=list(
    width="100%", height=700
  )
)
&#96;&#96;&#96;
</code></pre>

Note that in all of R code chunks above the `echo = FALSE` attribute is used. This is to prevent the R code within the chunk from rendering in the document alongside the Shiny components.

## Advanced Topics

### Shiny Reactives

Shiny Docs can also contain reactive expressions (useful when a piece of dynamic data is used in several places). As in Shiny applications, these values respond to changes in their inputs.

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo = FALSE}
selectInput("dataset", "Choose Dataset:", c("cars", "iris", "mtcars"))

activeDataset <- reactive({
  get(input$dataset, pos="package:datasets", inherits=FALSE)
})

renderTable({
  head(activeDataset(), 5)
})

renderPlot({
  plot(activeDataset())
})
&#96;&#96;&#96;
</code></pre>

Note that reactive expressions can be used anywhere, including in the definition of inline Shiny applications using the `shinyApp` function. To learn more about reactive expressions, see the [Shiny Tutorial](http://shiny.rstudio.com/articles/basics.html).

### Multiple Pages
 
You can link to other Shiny Doc by using the markdown link syntax and specifying the *relative* path to the document, e.g. `[Another Shiny Document](another.Rmd)`.
 
Currently, only one document can be active at a time, so documents can't easily share state (although some primitive global sharing is possible via `global.R`; see the help for `rmarkdown::run`). 
 
By default it's only possible to link to R Markdown files in the same directory subtree as the file on which `rmarkdown::run` was invoked (i.e you can't link to `../foo.rmd`.) You can use the `dir` argument to `rmarkdown::run` to indicate the directory to treat as the root. 

### Shiny Widgets

It's also possible to create re-usable Shiny widgets that enable authors to embed a Shiny application within a page with a single function call. For example, the following code could be used to embed a K Means clustering application:

```r
kmeans_cluster(iris)
```

This is what the widget would look like inside a running document:

![Shiny Widget KMeans](images/shiny-widget-kmeans.png)


See the article on [Shiny Widgets](developer_shiny_widgets.html) for additional details.

### Converting Existing Documents 

The getting started example demonstrated creating a brand new Shiny Doc. However, any R Markdown output format that produces HTML can be converted into a Shiny Doc. To convert and existing document:
 
- Add `runtime: shiny` to its YAML front matter. 

- Render it with `rmarkdown::run` instead of `rmarkdown::render`. 

For example, here's the front matter for a Shiny [html_document](html_document_format.html):

```yaml
---
title: "My Document"
output: html_document
runtime: shiny
---
```

And here's the front matter for a Shiny [ioslides_presentation](ioslides_presentation_format.html):

```yaml
---
title: "My Document"
output: ioslides_presentation
runtime: shiny
---
```

As described above, once you've added `runtime: shiny` to the document you can run it using either the **Run Document** command in RStudio or using the `rmarkdown::run` function. By default, documents are re-rendered on save, so once you've got a browser open with the document loaded, just save the R Markdown file to see your changes.
