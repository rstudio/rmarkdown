---
title: "Dygraphs"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
---

```{r setup, include=FALSE}
library(dygraphs)
library(flexdashboard)
```

### Lung Deaths (All)

```{r}
dygraph(ldeaths)
```

### Lung Deaths (Male)

```{r}
dygraph(mdeaths)
```

### Lung Deaths (Female)

```{r}
dygraph(fdeaths)
```
