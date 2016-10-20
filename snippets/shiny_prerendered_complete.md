---
title: "Old Faithful Eruptions"
output: flexdashboard::flex_dashboard
runtime: shiny_prerendered
---

```{r setup, include=FALSE}
library(dplyr)
knitr::opts_chunk$set(echo = FALSE)
```

```{r data, include=FALSE}
faithful_data <- sample_n(faithful, 100)
```

Column {.sidebar}
-------------------------------------------------------------

```{r}
selectInput("n_breaks", label = "Number of bins:",
            choices = c(10, 20, 35, 50), selected = 20)

sliderInput("bw_adjust", label = "Bandwidth adjustment:",
            min = 0.2, max = 2, value = 1, step = 0.2)
```

Column
-------------------------------------------------------------

### Geyser Eruption Duration

```{r}
plotOutput("eruptions")
```

```{r, context="server"}
output$eruptions <- renderPlot({
  hist(faithful_data$eruptions, probability = TRUE,
       breaks = as.numeric(input$n_breaks),
       xlab = "Duration (minutes)", main = "Geyser Eruption Duration")

  dens <- density(faithful_data$eruptions, adjust = input$bw_adjust)
  lines(dens, col = "blue")
})
```