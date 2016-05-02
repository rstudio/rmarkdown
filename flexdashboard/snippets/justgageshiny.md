### Contact Rate

```{r}
renderGauge({
  rate <- computeContactRate(input$region)
  gauge(rate, min = 0, max = 100, symbol = '%', gaugeSectors(
    success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
  ))
})
```
