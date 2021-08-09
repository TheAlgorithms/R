

```r
oneHotEncode <- function(x, fullRank = T){
    if(fullRank){
        return(model.matrix(~ 0 + ., data = x))
    } else {
        charCols <- colnames(x)[sapply(x, is.character)]
        if(length(charCols) > 0){
            for(col in charCols){
                x[[eval(col)]] <- factor(x[[eval(col)]])
            }
        }
        factorCols <- colnames(x)[sapply(x, is.factor)]
        contrastsList <- vector(mode = "list", length = length(factorCols))
        names(contrastsList) <- factorCols
        if(length(factorCols) > 0){
            for(col in factorCols){
                contrastsList[[eval(col)]] <- contrasts(x[[eval(col)]], contrasts = F)
            }
            return(model.matrix(~ 0 + ., data = x, contrasts = contrastsList))
        } else {
            return(model.matrix(~ 0 + ., data = x))
        }
    }
}

diamonds <- ggplot2::diamonds
```

```
## Error in loadNamespace(x): there is no package called 'ggplot2'
```

```r
head(oneHotEncode(diamonds))
```

```
## Error in terms.formula(object, data = data): object 'diamonds' not found
```

```r
head(oneHotEncode(diamonds, fullRank = F))
```

```
## Error in is.data.frame(x): object 'diamonds' not found
```

