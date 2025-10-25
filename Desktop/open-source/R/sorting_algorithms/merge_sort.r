# Merge sort in R:

merge.func <-function(leftArray, rightArray) {
    l <- numeric(length(leftArray) + length(rightArray))
    leftIndex <- 1; rightIndex <- 1; i <- 1;
    for(i in 1:length(l)) {
        if((leftIndex <= length(leftArray) && leftArray[leftIndex] < rightArray[rightIndex]) || rightIndex > length(rightArray)) {
            l[i] <- leftArray[leftIndex]
            leftIndex <- leftIndex + 1
        } else {
            l[i] <- rightArray[rightIndex]
            rightIndex <- rightIndex + 1
        }
    }
    return(l)
}

merge.sort <- function(elements.vec) {
    if(length(elements.vec) > 1) { 
        m <- ceiling(length(elements.vec) / 2)
        leftArray <- merge.sort(elements.vec[1:m])
        rightArray <- merge.sort(elements.vec[(m + 1):length(elements.vec)])
        merge.func(leftArray, rightArray)
    } 
    else {
        return(elements.vec)
    }
}

# Example:
# merge.sort(c(5, 2, 3, 1, 4)) 
# [1] 1 2 3 4 5
