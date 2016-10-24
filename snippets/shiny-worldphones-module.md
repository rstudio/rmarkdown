# UI function
worldPhonesUI <- function(id, selected = "Europe", height = 600) {
  ns <- NS(id)
  fillCol(height = height, flex = c(NA, 1), 
    inputPanel(
      selectInput(ns("region"), "Region:", 
                  choices = colnames(WorldPhones),
                  selected = selected)
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