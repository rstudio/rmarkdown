---
title: "Shiny Widgets"
output: html_document
---

Shiny widgets enable you to create re-usable Shiny components that are included within an R Markdown document using a single function call. Shiny widgets can also be invoked directly from the console (useful during authoring) and show their output within the RStudio Viewer pane or an external web browser.

#### Prerequisties

Creating Shiny widgets requires development versions of the **knitr**, **shiny**, and **rmarkdown** packages, which you can install as follows:

```r
devtools::install_github(c("yihui/knitr", "rstudio/shiny", "rstudio/rmarkdown"))
```

## Creating a Shiny Widget

At their core Shiny widgets are mini-applications created using the `shinyApp` function. Rather than create a `ui.R` and `server.R` as you would for a typical Shiny application, you pass the ui and server definitions to the `shinyApp` function as arguments. For example:

```r
shinyApp(
  
  ui = fluidPage(
    selectInput("region", "Region:", choices = colnames(WorldPhones)),
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
```

The simplest type of Shiny widget is just an R function that returns a `shinyApp`.

### Example

The [**rmdexamples**](https://github.com/rstudio/rmdexamples) package includes an example of Shiny widget implemented in this fashion. The [`kmeans_cluster`](https://github.com/rstudio/rmdexamples/blob/master/R/kmeans_cluster.R) function takes a single "dataset" argument and returns a Shiny widget. You can use it within an R Markdown document like this:

<pre class="markdown"><code>&#96;&#96;&#96;{r, echo = FALSE}
library(rmdexamples)
kmeans_cluster(iris)
&#96;&#96;&#96;
</code></pre>

This is what the widget looks like inside a running document:

![Shiny Widget KMeans](images/shiny-widget-kmeans.png)

#### Source Code

Here's the source code for the `kmeans_cluster` function. Note the use of the `height` option to control the default height of the widget when rendered inside a document.

```r
kmeans_cluster <- function(dataset) { 
  
  require(shiny)  
  
  shinyApp(
  
    ui = fluidPage(
      wellPanel(fluidRow(
        column(4, selectInput('xcol', 'X Variable', names(dataset))),
        column(4, selectInput('ycol', 'Y Variable', names(dataset),
                              selected=names(dataset)[[2]])),
        column(4, numericInput('clusters', 'Cluster count', 3,
                               min = 1, max = 9))
      )),
      fluidRow(
        plotOutput('kmeans')  
      )
    ),
    
    server = function(input, output, session) {
      
      # Combine the selected variables into a new data frame
      selectedData <- reactive({
        dataset[, c(input$xcol, input$ycol)]
      })
      
      clusters <- reactive({
        kmeans(selectedData(), input$clusters)
      })
      
      output$kmeans <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
      })
    },
    
    options = list(height = 550)
  )
}
```
