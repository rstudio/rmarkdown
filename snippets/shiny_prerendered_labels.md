```{r setup, include=FALSE}
library(dplyr)
knitr::opts_chunk$set(echo = FALSE)
```

```{r data, include=FALSE}
dataset <- import_data("data.csv")
dataset <- sample_n(dataset, 1000)
```