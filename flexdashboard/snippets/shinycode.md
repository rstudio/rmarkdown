---
title: "Deaths from Lung Cancer"
output: flexdashboard::flex_dashboard
runtime: shiny
---

```{r setup, include=FALSE}
library(dygraphs)
```

Column {.sidebar}
-----------------------------------------------------------------------

```{r}
numericInput("months", label = "Months to Predict",
              value = 72, min = 12, max = 144, step = 12)

selectInput("interval", label = "Prediction Interval",
            choices = c("0.80", "0.90", "0.95", "0.99"),
            selected = "0.95")

checkboxInput("showgrid", label = "Show Grid", value = TRUE)
```

Monthly deaths from bronchitis, emphysema and asthma in the UK, 1974–1979.


Column
-----------------------------------------------------------------------

### Predicted Deaths/Month

```{r}
predicted <- reactive({
  hw <- HoltWinters(ldeaths)
  predict(hw, n.ahead = input$months, prediction.interval = TRUE,
          level = as.numeric(input$interval))
})

renderDygraph({
  dygraph(predicted()) %>%     
    dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
    dyOptions(drawGrid = input$showgrid)
})
```
