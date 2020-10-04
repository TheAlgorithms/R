# Heap sort in R:

build.heap <- function(elements.vec) {
  l = length(elements.vec)
  heap = elements.vec
  for (i in l:1) {
    heap = modify.heap(heap, i)
  }
  return(heap)
}

is.heap <- function(heap, rootIndex) {
  i = rootIndex
  res = T
  while(2 * i <= length(heap) & res) {
    child = c(heap[2 * i], heap[2 * i + 1])
    child = child[!is.na(child)]
    result.bool = all(heap[i] <= child)
    i = i + 1
  }
  return(result.bool)
}

modify.heap <- function(heap, rootIndex) {
  l = length(heap)
  flag = 1  
  while (rootIndex * 2 <= l && flag == 1) {
    leftIndex = rootIndex * 2
    rightIndex = rootIndex * 2 + 1
    flag = 0
    child = c(heap[leftIndex], heap[rightIndex])
    child = child[!is.na(child)]
    minIndex = which.min(child)
    if (heap[rootIndex] > child[minIndex]) {
      flag = 1
      heapIndex = c(leftIndex, rightIndex)[minIndex]
      temp = heap[heapIndex]
      heap[heapIndex] = heap[rootIndex]
      heap[rootIndex] = temp
      rootIndex = heapIndex
    }
  }
  return(heap)
}

heap.sort <- function(heap) {
  sorted.elements = NULL
  l = length(heap)
  while(l > 0)
  {
    sorted.elements = c(sorted.elements, heap[1])
    l = length(heap)
    heap[1] = heap[l]
    heap = heap[1:(l - 1)]
    heap = modify.heap(heap, rootIndex = 1)
    l = l - 1
  }
  return(sorted.elements)
}

# Example:
# heap.sort(build.heap(c(5, 2, 3, 1, 4))) 
# [1] 1 2 3 4 5
