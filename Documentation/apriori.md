

```r
library(arules)
```

```
## Error in library(arules): there is no package called 'arules'
```

```r
groceries <- read.transactions("groceries.csv", sep=",")
```

```
## Error in read.transactions("groceries.csv", sep = ","): could not find function "read.transactions"
```

```r
summary(groceries)
```

```
## Error in summary(groceries): object 'groceries' not found
```

```r
itemFrequencyPlot(groceries, topN=20)
```

```
## Error in itemFrequencyPlot(groceries, topN = 20): could not find function "itemFrequencyPlot"
```

```r
#sample for randomly extracting samples, image function for visualing sparse matrix
image(sample(groceries,100))
```

```
## Error in sample(groceries, 100): object 'groceries' not found
```

```r
groceries_rule <- apriori(data=groceries, parameter=list(support=0.006, confidence=0.25, minlen=2))
```

```
## Error in apriori(data = groceries, parameter = list(support = 0.006, confidence = 0.25, : could not find function "apriori"
```

```r
plotly_arules(groceries_rule)
```

```
## Error in plotly_arules(groceries_rule): could not find function "plotly_arules"
```

```r
summary(groceries_rule)
```

```
## Error in summary(groceries_rule): object 'groceries_rule' not found
```

