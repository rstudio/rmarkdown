### Articles per Day

```{r}
renderValueBox({
  articles <- computeArticles(input$types)
  valueBox(articles, 
           icon = "fa-pencil",
           color = ifelse(articles > 100, "success", "info"))
})
```
