# Linked List Algorithms

This directory contains comprehensive implementations of various linked list data structures in R. Linked lists are fundamental linear data structures where elements are stored in nodes, and each node contains data and reference(s) to other nodes.

## 📋 Contents

- [`singly_linked_list.r`](singly_linked_list.r) - Singly Linked List implementation
- [`doubly_linked_list.r`](doubly_linked_list.r) - Doubly Linked List implementation  
- [`circular_linked_list.r`](circular_linked_list.r) - Circular Linked List implementation
- [`README.md`](README.md) - This documentation file

## 🔗 Linked List Types Overview

### 1. Singly Linked List
**Structure**: Each node contains data and a pointer to the next node
**Traversal**: Forward only (head → tail)
**Memory**: Minimal overhead (one pointer per node)

**Use Cases**:
- Implementing stacks and queues
- Undo functionality in applications
- Simple sequential data storage

### 2. Doubly Linked List  
**Structure**: Each node contains data and pointers to both next and previous nodes
**Traversal**: Bidirectional (forward and backward)
**Memory**: Higher overhead (two pointers per node)

**Use Cases**:
- Browser history navigation (back/forward)
- LRU cache implementation
- Complex navigation systems

### 3. Circular Linked List
**Structure**: Last node points back to the first node (forms a circle)
**Traversal**: Infinite circular traversal possible
**Memory**: Same as singly linked (one pointer per node)

**Use Cases**:
- Round-robin CPU scheduling
- Multiplayer turn-based games
- Continuous playlists

## ⚡ Performance Comparison

| Operation | Singly | Doubly | Circular |
|-----------|--------|--------|----------|
| Insert at Head | O(1) | O(1) | O(1) |
| Insert at Tail | O(n) | O(1) | O(n) |
| Delete from Head | O(1) | O(1) | O(1) |
| Delete from Tail | O(n) | O(1) | O(n) |
| Search | O(n) | O(n) | O(n) |
| Access by Index | O(n) | O(n/2)* | O(n) |

*Doubly linked list can traverse from closer end (head or tail)

## 🚀 Quick Start Guide

### Installation
No additional packages required! Just R base installation with `setRefClass` support.

### Basic Usage Examples

#### Singly Linked List
```r
source("singly_linked_list.r")

# Create new list
my_list <- SinglyLinkedList$new()

# Basic operations
my_list$insert_at_head(10)
my_list$insert_at_tail(20)
my_list$insert_at_position(15, 1)
my_list$print_list()

# Access and search
value <- my_list$get(1)              # Get element at position 1
position <- my_list$search(15)       # Find position of value 15
vector_form <- my_list$to_vector()   # Convert to R vector

# Delete operations
my_list$delete_from_head()
my_list$delete_by_value(15)
```

#### Doubly Linked List
```r
source("doubly_linked_list.r")

# Create new doubly linked list
dll <- DoublyLinkedList$new()

# Bidirectional operations
dll$insert_at_head(10)
dll$insert_at_tail(20)
dll$print_list()                    # Forward traversal
dll$print_list_reverse()            # Backward traversal

# Optimized access (traverses from closer end)
dll$get(0)                          # Accessed from head
dll$get(dll$get_size() - 1)         # Accessed from tail

# Convert to vectors
forward_vector <- dll$to_vector()
backward_vector <- dll$to_vector_reverse()
```

#### Circular Linked List
```r
source("circular_linked_list.r")

# Create new circular linked list  
cll <- CircularLinkedList$new()

# Build the circle
cll$insert_at_tail(1)
cll$insert_at_tail(2)
cll$insert_at_tail(3)

# Demonstrate circular nature
cll$traverse_n_times(2)             # Go around circle twice

# Solve classic problems
survivor <- cll$josephus_problem(3)  # Josephus problem with k=3
```

## 🎯 Advanced Features

### Comprehensive Operations
All implementations include:
- **Insertion**: at head, tail, or any position
- **Deletion**: from head, tail, position, or by value
- **Search**: find element position
- **Access**: get element by position
- **Utilities**: size, empty check, clear, convert to vector

### Algorithm Implementations
- **Middle element detection** (two-pointer technique)
- **Cycle detection** (Floyd's algorithm) 
- **List reversal** (in-place)
- **Josephus problem** (circular list specialty)

### Educational Features  
- **Step-by-step explanations** with console output
- **Time/space complexity analysis** in comments
- **Real-world examples** and use cases
- **Interactive demonstrations** for each type

## 🔬 Detailed Examples

### Running the Demonstrations
Each implementation includes comprehensive examples:

```r
# Singly Linked List examples
source("singly_linked_list.r")
demonstrate_singly_linked_list()

# Doubly Linked List examples  
source("doubly_linked_list.r")
demonstrate_doubly_linked_list()

# Circular Linked List examples
source("circular_linked_list.r") 
demonstrate_circular_linked_list()
```

### Real-World Applications

#### 1. Student Grade Management (Singly Linked List)
```r
grades_list <- SinglyLinkedList$new()
grades <- c(85, 92, 78, 96, 83, 89)

# Add grades
for (grade in grades) {
  grades_list$insert_at_tail(grade)
}

# Calculate statistics
grade_vector <- grades_list$to_vector()
average <- mean(grade_vector)
```

#### 2. Browser History (Doubly Linked List)
```r
history <- DoublyLinkedList$new()

# Visit pages
history$insert_at_tail("google.com")
history$insert_at_tail("github.com") 
history$insert_at_tail("stackoverflow.com")

# Go back (delete current)
current_page <- history$delete_from_tail()

# Go forward (add new page)
history$insert_at_tail("wikipedia.org")
```

#### 3. Round-Robin Scheduling (Circular Linked List)
```r
scheduler <- CircularLinkedList$new()

# Add processes
processes <- c("Process_A", "Process_B", "Process_C")
for (process in processes) {
  scheduler$insert_at_tail(process)
}

# Simulate time slots
current <- scheduler$head
for (slot in 1:6) {
  cat("Time slot", slot, ":", current$data, "\n")
  current <- current$next_node
}
```

## 🧮 Algorithm Analysis

### Space Complexity
- **Singly**: O(n) space, 1 pointer per node
- **Doubly**: O(n) space, 2 pointers per node  
- **Circular**: O(n) space, 1 pointer per node

### When to Use Each Type

| Requirement | Best Choice | Reason |
|-------------|-------------|---------|
| Memory efficiency | Singly | Minimal pointer overhead |
| Bidirectional traversal | Doubly | Two-way navigation |
| Cyclic operations | Circular | Natural circular structure |
| Frequent tail operations | Doubly | O(1) tail access |
| Simple implementation | Singly | Easiest to understand/debug |
| LRU Cache | Doubly | Efficient insertion/deletion anywhere |
| Round-robin algorithms | Circular | Natural fit for cyclic scheduling |

## ⚠️ Important Considerations

### Common Pitfalls
1. **Memory Leaks**: Ensure proper node cleanup in languages with manual memory management
2. **Infinite Loops**: In circular lists, always track traversal completion
3. **Null Pointer Exceptions**: Check for empty lists before operations
4. **Position Bounds**: Validate position parameters in insert/delete operations

### Best Practices  
1. **Always validate inputs** (positions, null checks)
2. **Maintain size counter** for O(1) size queries
3. **Use appropriate type** based on access patterns
4. **Test edge cases** (empty list, single element, position bounds)
5. **Document time complexities** for each operation

## 📚 Further Reading

### Recommended Algorithms to Implement Next
- **Stack using Linked List**: LIFO operations
- **Queue using Linked List**: FIFO operations  
- **Deque using Doubly Linked List**: Double-ended queue
- **LRU Cache using Doubly Linked List**: Cache replacement policy
- **Skip List**: Probabilistic data structure for fast search

### Advanced Topics
- **Lock-free Linked Lists**: Concurrent programming
- **Memory Pool Allocation**: Efficient node management
- **Template/Generic Implementations**: Type-safe containers
- **Persistent Linked Lists**: Functional programming approach

## 🤝 Contributing

We welcome contributions! Consider adding:
- Additional linked list variants (XOR linked list, skip list)
- Performance benchmarking utilities
- Visualization functions (ASCII art display)
- More real-world application examples
- Memory usage analysis tools

### Development Guidelines
1. Follow existing code style and documentation standards
2. Include comprehensive test cases and examples
3. Add time/space complexity analysis in comments
4. Provide practical application demonstrations
5. Ensure no automatic side effects (examples available but not auto-run)

## 📄 License

This code is provided for educational purposes. Please cite appropriately if used in academic work.

---

*"A linked list is a linear collection of data elements whose order is not given by their physical placement in memory."*

These implementations provide a solid foundation for understanding linked lists and can serve as building blocks for more complex data structures and algorithms!