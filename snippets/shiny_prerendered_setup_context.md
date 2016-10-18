```{r, context="setup", include=FALSE}
# load libraries
library(dplyr)

# set options/configuration
knitr::opts_chunk$set(echo = FALSE)

# load data
dataset <- import_data("data.csv")
dataset <- sample_n(dataset, 1000)
```