```{r, context="data", cache=TRUE, cache.extra=file.info("data.csv")$mtime}
# load data
dataset <- import_data("data.csv")
dataset <- sample_n(dataset, 1000)
```