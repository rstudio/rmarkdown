### Cars

```{r}
DT::renderDataTable({
  data <- head(mtcars, n = input$maxrows)
  DT::datatable(data, options = list(
    bPaginate = FALSE
  ))
})
```
