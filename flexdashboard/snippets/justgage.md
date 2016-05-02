Row 
-----------------------------------------------------------------------

### Contact Rate

```{r}
rate <- computeContactRate()
gauge(rate, min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
))
```

### Average Rating 

```{r}
rating <- computeAverageRating()
gauge(rating, min = 0, max = 50, gaugeSectors(
  success = c(41, 50), warning = c(21, 40), danger = c(0, 20)
))
```

### Cancellations

```{r}
cancellations <- computeCancellations()
gauge(cancellations, min = 0, max = 10, gaugeSectors(
  success = c(0, 2), warning = c(3, 6), danger = c(7, 10)
))
```
