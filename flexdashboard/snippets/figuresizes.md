---
title: "Figure Sizes"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
---

Row
-------------------------------------

### Chart 1

```{r, fig.width=10, fig.height=7}
plot(cars)
```

Row
-------------------------------------
    
### Chart 2
    
```{r, fig.width=5, fig.height=5}
plot(pressure)
```
    
### Chart 3

```{r, fig.width=5, fig.height=5}
plot(airmiles)
```
