---
title: "World Phones"
output: html_document
runtime: shiny_prerendered
---

```{r setup, include=FALSE}
source("worldPhones.R")
```

```{r, echo=FALSE}
worldPhonesUI("phones", selected = "Asia", height = 500)
```

```{r, context="server"}
callModule(worldPhonesServer, "phones")
```
