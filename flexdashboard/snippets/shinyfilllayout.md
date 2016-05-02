```{r}
fillCol(height = 600, flex = c(NA, 1), 
  inputPanel(
    selectInput("region", "Region:", choices = colnames(WorldPhones))
  ),
  plotOutput("phonePlot", height = "100%")
)

output$phonePlot <- renderPlot({
  barplot(WorldPhones[,input$region]*1000, 
          ylab = "Number of Telephones", xlab = "Year")
})
```