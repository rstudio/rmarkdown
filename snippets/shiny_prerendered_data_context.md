```{r, context="data", include=FALSE}
# load data
dataset <- import_data("data.csv")
dataset <- sample_n(dataset, 1000)
```