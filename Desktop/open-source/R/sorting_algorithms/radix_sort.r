# Radix sort in R:

radix.sort <- function(elements.vec) {
    x <- nchar(max(elements.vec))
    for (i in 1:x)
        elements.vec <- elements.vec[order(elements.vec %% (10 ^ i))]
    return(elements.vec)
}

# Example:
# radix.sort(c(50, 3200, 27, 976, 820)) 
# [1] 27 50 820 976 3200

# Note:
# It is implemented in the 'sort' function of base R:
# sort(c(50, 3200, 27, 976, 820), method = "radix" , index.return = FALSE)
# [1] 27 50 820 976 3200
