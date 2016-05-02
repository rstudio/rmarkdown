---
title: "Text Annotations"
output:
  flexdashboard::flex_dashboard:
    orientation: rows
---

Monthly deaths from bronchitis, emphysema and asthma in the
UK, 1974–1979 (Source: P. J. Diggle, 1990, Time Series: A
Biostatistical Introduction. Oxford, table A.3)

```{r setup, include=FALSE}
library(dygraphs)
```

Row {data-height=600}
-------------------------------------

### All Lung Deaths

```{r}
dygraph(ldeaths)
```

Row {data-height=400}
-------------------------------------

### Male Deaths

```{r}
dygraph(mdeaths)
```

### About dygraphs

This example makes use of the dygraphs R package. The dygraphs
package provides rich facilities for charting time-series data 
in R. You can use dygraphs at the R console, within R Markdown
documents, and within Shiny applications.
