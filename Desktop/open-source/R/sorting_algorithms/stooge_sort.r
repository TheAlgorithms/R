# Stooge sort in R:

stooge.sort <- function(elements.vec) {
	i = 1
	j = length(elements.vec)
	if (elements.vec[j] < elements.vec[i]) elements.vec[c(j, i)] = elements.vec[c(i, j)]
	if (j - i > 1) {
		t = (j - i + 1) %/% 3
		elements.vec[i:(j - t)] = stooge.sort(elements.vec[i:(j - t)])
		elements.vec[(i + t):j] = stooge.sort(elements.vec[(i + t):j])
		elements.vec[i:(j - t)] = stooge.sort(elements.vec[i:(j - t)])
	}
	elements.vec
}
 
# Example:
# stooge.sort(sample(21, 20))
# [1] 1  2  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
