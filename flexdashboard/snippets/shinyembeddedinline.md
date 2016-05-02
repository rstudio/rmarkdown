```{r}
shinyApp(
  ui = fillPage(
    fillCol(flex = c(NA, 1), 
      inputPanel(
        selectInput("region", "Region:", choices = colnames(WorldPhones))
      ),
      plotOutput("phonePlot", height = "100%")
    )
  ),
  server = function(input, output) {
    output$phonePlot <- renderPlot({
      barplot(WorldPhones[,input$region]*1000, 
              ylab = "Number of Telephones", xlab = "Year")
    })
  },
  options = list(height = 600)
)
```