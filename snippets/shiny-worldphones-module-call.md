---
title: "World Phones"
output: html_document
runtime: shiny_prerendered
---

```{r setup, include=FALSE}
source("worldPhones.R")
```

```{r, echo=FALSE}
shiny_module(worldPhones, selected = "Asia", height = 500)
```
