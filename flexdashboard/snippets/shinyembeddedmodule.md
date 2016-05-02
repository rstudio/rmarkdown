# Shiny worldPhones module 

# UI function
worldPhonesUI <- function(id) {
  ns <- NS(id)
  fillCol(height = 600, flex = c(NA, 1), 
    inputPanel(
      selectInput(ns("region"), "Region:", choices = colnames(WorldPhones))
    ),
    plotOutput(ns("phonePlot"), height = "100%")
  )
}

# Server function
worldPhones <- function(input, output, session) {
  output$phonePlot <- renderPlot({
    barplot(WorldPhones[,input$region]*1000, 
            ylab = "Number of Telephones", xlab = "Year")
  })
}