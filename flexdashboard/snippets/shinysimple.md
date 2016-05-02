sliderInput("bins", "Number of bins:", 
            min = 1, max = 50, value = 30)

renderPlot({
  hist(faithful[, 2], breaks = input$bins)
})