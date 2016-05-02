---
title: "Mobile Specific"
output: flexdashboard::flex_dashboard
---

### Chart 1
    
```{r}
plot(cars)
```
    
### Chart 2 {.no-mobile}

```{r}
plot(pressure)
```

## Chart 3 

```{r}
plot(mtcars)
```

## Chart 3 {.mobile}

```{r}
plot(mtcars)
```
