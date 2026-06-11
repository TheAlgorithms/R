# A binary heap is a complete binary tree where each node is smaller than or
# equal to its children (min-heap). The minimum is always at the root.
#
# To insert a value, add it as the last node and swap it upward
# until the heap property holds (sift up).
#
# To remove the minimum, move the last node to the root and swap it
# downward until the heap property holds (sift down).
#
# Time Complexities:
# - Push (insert):  O(log n)
# - Pop (extract min): O(log n)
# - Peek: O(1)
#
# Space Complexity: O(n)
#
# Applications:
# - Priority queues (task scheduling, process management)
# - Dijkstra's shortest path algorithm
# - Heap sort
# - Finding the k smallest/largest elements (Top-K)
# - Event-driven simulation
#
# Example usage:
# h <- c()                 # create an empty heap
# h <- push(h, 5)          # insert values
# h <- push(h, 3)
# h <- push(h, 8)
# peek(h)                  # view the minimum (3) without removing
# res <- pop(h)            # extract the minimum
# res$value                # -> 3 (the minimum that was removed)
# res$heap                 # -> the heap after removal


push <- function(heap, value) {
  
  heap <- c(heap, value)
  i <- length(heap)
  
  while (i > 1) {
    
    p <- i %/% 2
    
    if (heap[p] > heap[i]){
      heap[c(p, i)] <- heap[c(i, p)]
      i <- p
    }
    else {
      break
    }
  }
  return(heap)
}

pop <- function(heap) {
  
  min_value <- heap[1]

  heap[1] <- heap[length(heap)]
  heap <- heap[-length(heap)]
  
  i <- 1
  child <- 0
  
  while (2 * i <= length(heap)){
    if ((2 * i + 1 <= length(heap) && heap[2 * i + 1] < heap[2 * i])){
      child <- 2 * i + 1
    }
    else {
      child <- 2 * i
    }
    
    if (heap[child] < heap[i]){
      heap[c(child, i)] <- heap[c(i, child)]
      i <- child
    }
    else {
      break
    }
  }
  
  return(list(value = min_value, heap = heap))
}

peek <- function(heap) {
  return(heap[1])
}

# Test

h <- c()
h <- push(h, 5)
h <- push(h, 3)
h <- push(h, 8)
h <- push(h, 1)
h <- push(h, 9)
h <- push(h, 2)

print(peek(h))    
print(h)

res <- pop(h)
print(res$value)  
print(res$heap) 
  

