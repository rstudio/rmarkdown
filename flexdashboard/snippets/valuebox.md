Row
-----------------------------------------------------------------------

### Articles per Day

```{r}
articles <- computeArticles()
valueBox(articles, icon = "fa-pencil")
```

### Comments per Day

```{r}
comments <- computeComments()
valueBox(comments, icon = "fa-comments")
```

### Spam per Day

```{r}
spam <- computeSpam()
valueBox(spam, 
         icon = "fa-trash",
         color = ifelse(spam > 10, "warning", "primary"))
```
