---
title: "Exclude on Mobile"
output: flexdashboard::flex_dashboard
---

### Chart 1
    
```{r}
plot(cars)
```
    
### Chart 2 {.no-mobile}

```{r}
plot(summary)
```
