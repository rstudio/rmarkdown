```{r, echo=FALSE}
a <- 10
textOutput("text")
```

```{r, context="server"}
output$text <- renderText({ a })
```