---
title: "Embedded Shiny Apps"
output:
  html_document:
    toc: true
---

It's possible to embed an entire Shiny application within a document. There are two syntaxes for this: 

1) Defining the application inline using the `shinyApp` function; or

2) Referring to an external application directory using the `shinyAppDir` function.

## Inline Applications

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

## External Applications

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
