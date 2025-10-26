# Stack Data Structure Implementation in R
#
# A stack is a linear data structure that follows the Last In First Out (LIFO) principle.
# Elements are added and removed from the same end, called the top of the stack.
#
# Time Complexities:
# - Push: O(1) - adding element to top
# - Pop: O(1) - removing element from top
# - Peek/Top: O(1) - viewing top element
# - Size: O(1) - getting stack size
# - IsEmpty: O(1) - checking if stack is empty
#
# Space Complexity: O(n) where n is number of elements
#
# Applications:
# - Function call management (call stack)
# - Expression evaluation and syntax parsing
# - Undo operations in text editors
# - Browser history (back button)
# - Depth-First Search (DFS) in graphs
# - Backtracking algorithms
# - Memory management in programming languages

# Define Stack class using Reference Classes
Stack <- setRefClass("Stack",
  fields = list(
    items = "list",
    top_index = "numeric",
    max_size = "numeric"
  ),
  methods = list(
    initialize = function(max_size = Inf) {
      "Initialize an empty stack with optional maximum size"
      .self$items <- list()
      .self$top_index <- 0
      .self$max_size <- max_size
      cat("Stack initialized with max size:", ifelse(is.infinite(max_size), "unlimited", max_size), "\n")
    },
    
    push = function(item) {
      "Add an element to the top of the stack"
      if (.self$size() >= .self$max_size) {
        stop("Stack overflow: Cannot push more elements. Max size reached: ", .self$max_size)
      }
      
      .self$top_index <- .self$top_index + 1
      .self$items[[.self$top_index]] <- item
      cat("Pushed:", item, "| Size:", .self$size(), "\n")
    },
    
    pop = function() {
      "Remove and return the top element from the stack"
      if (.self$is_empty()) {
        stop("Stack underflow: Cannot pop from empty stack")
      }
      
      item <- .self$items[[.self$top_index]]
      .self$items[[.self$top_index]] <- NULL
      .self$top_index <- .self$top_index - 1
      
      # Clean up the list to optimize memory
      if (.self$top_index < length(.self$items)) {
        .self$items <- .self$items[1:.self$top_index]
      }
      
      cat("Popped:", item, "| Size:", .self$size(), "\n")
      return(item)
    },
    
    peek = function() {
      "Return the top element without removing it"
      if (.self$is_empty()) {
        stop("Stack is empty: No top element")
      }
      return(.self$items[[.self$top_index]])
    },
    
    top = function() {
      "Alias for peek() - return the top element without removing it"
      return(.self$peek())
    },
    
    is_empty = function() {
      "Check if the stack is empty"
      return(.self$top_index == 0)
    },
    
    is_full = function() {
      "Check if the stack is full (only applicable for bounded stacks)"
      return(.self$size() >= .self$max_size)
    },
    
    size = function() {
      "Return the number of elements in the stack"
      return(.self$top_index)
    },
    
    clear = function() {
      "Remove all elements from the stack"
      .self$items <- list()
      .self$top_index <- 0
      cat("Stack cleared\n")
    },
    
    display = function() {
      "Display all elements in the stack from bottom to top"
      if (.self$is_empty()) {
        cat("Stack is empty: []\n")
        return()
      }
      
      elements <- character(0)
      for (i in 1:.self$top_index) {
        elements <- c(elements, as.character(.self$items[[i]]))
      }
      cat("Stack: [", paste(elements, collapse = " | "), "] (bottom → top)\n")
    },
    
    to_vector = function() {
      "Convert stack to vector (bottom to top order)"
      if (.self$is_empty()) return(c())
      
      result <- c()
      for (i in 1:.self$top_index) {
        result <- c(result, .self$items[[i]])
      }
      return(result)
    },
    
    search = function(item) {
      "Search for an item in the stack and return its position from top (1-indexed)"
      if (.self$is_empty()) return(-1)
      
      for (i in .self$top_index:1) {
        if (identical(.self$items[[i]], item)) {
          return(.self$top_index - i + 1)  # Position from top
        }
      }
      return(-1)  # Item not found
    },
    
    reverse = function() {
      "Reverse the order of elements in the stack"
      if (.self$size() <= 1) return()
      
      # Create a temporary vector and reverse it
      temp_items <- .self$to_vector()
      temp_items <- rev(temp_items)
      
      # Rebuild stack with reversed order
      .self$clear()
      for (item in temp_items) {
        .self$push(item)
      }
      cat("Stack reversed\n")
    }
  )
)

# Utility function to demonstrate stack operations
demonstrate_stack_operations <- function() {
  cat("\n=== Stack Data Structure Demonstration ===\n\n")
  
  # Create a stack with maximum size of 5
  s <- Stack$new(max_size = 5)
  
  cat("\n1. Basic Push Operations:\n")
  s$push("A")
  s$push("B")
  s$push("C")
  s$display()
  
  cat("\n2. Stack Status:\n")
  cat("Size:", s$size(), "\n")
  cat("Is Empty:", s$is_empty(), "\n")
  cat("Is Full:", s$is_full(), "\n")
  cat("Top element:", s$peek(), "\n")
  
  cat("\n3. Pop Operations:\n")
  s$pop()
  s$display()
  s$pop()
  s$display()
  
  cat("\n4. More Push Operations:\n")
  s$push("D")
  s$push("E")
  s$push("F")
  s$push("G")
  s$display()
  
  cat("\n5. Search Operations:\n")
  cat("Position of 'E' from top:", s$search("E"), "\n")
  cat("Position of 'Z' from top:", s$search("Z"), "\n")
  
  cat("\n6. Stack to Vector:\n")
  vec <- s$to_vector()
  cat("As vector:", paste(vec, collapse = ", "), "\n")
  
  cat("\n7. Reverse Stack:\n")
  s$reverse()
  s$display()
  
  cat("\n8. Testing Stack Overflow:\n")
  tryCatch({
    s$push("H")  # This should cause overflow
  }, error = function(e) {
    cat("Error caught:", e$message, "\n")
  })
  
  cat("\n9. Clear Stack:\n")
  s$clear()
  s$display()
  
  cat("\n10. Testing Stack Underflow:\n")
  tryCatch({
    s$pop()  # This should cause underflow
  }, error = function(e) {
    cat("Error caught:", e$message, "\n")
  })
}

# Expression Evaluator using Stack (bonus feature)
ExpressionEvaluator <- setRefClass("ExpressionEvaluator",
  fields = list(
    operators = "character",
    precedence = "list"
  ),
  methods = list(
    initialize = function() {
      "Initialize expression evaluator with operator precedence"
      .self$operators <- c("+", "-", "*", "/", "^", "(", ")")
      .self$precedence <- list("+" = 1, "-" = 1, "*" = 2, "/" = 2, "^" = 3, "(" = 0, ")" = 0)
      cat("Expression Evaluator initialized\n")
    },
    
    is_operator = function(char) {
      "Check if character is an operator"
      return(char %in% .self$operators)
    },
    
    get_precedence = function(op) {
      "Get precedence of an operator"
      return(.self$precedence[[op]])
    },
    
    infix_to_postfix = function(expression) {
      "Convert infix expression to postfix notation"
      # Remove spaces and split into tokens
      tokens <- strsplit(gsub(" ", "", expression), "")[[1]]
      result <- character(0)
      stack <- Stack$new()
      
      for (token in tokens) {
        if (!.self$is_operator(token)) {
          # Operand
          result <- c(result, token)
        } else if (token == "(") {
          stack$push(token)
        } else if (token == ")") {
          while (!stack$is_empty() && stack$peek() != "(") {
            result <- c(result, stack$pop())
          }
          if (!stack$is_empty()) stack$pop()  # Remove the '('
        } else {
          # Operator
          while (!stack$is_empty() && 
                 .self$get_precedence(stack$peek()) >= .self$get_precedence(token)) {
            result <- c(result, stack$pop())
          }
          stack$push(token)
        }
      }
      
      # Pop remaining operators
      while (!stack$is_empty()) {
        result <- c(result, stack$pop())
      }
      
      return(paste(result, collapse = " "))
    },
    
    evaluate_postfix = function(expression) {
      "Evaluate postfix expression"
      tokens <- strsplit(expression, " ")[[1]]
      stack <- Stack$new()
      
      for (token in tokens) {
        if (!.self$is_operator(token)) {
          # Operand
          stack$push(as.numeric(token))
        } else {
          # Operator
          if (stack$size() < 2) {
            stop("Invalid expression: insufficient operands")
          }
          
          b <- stack$pop()
          a <- stack$pop()
          
          result <- switch(token,
            "+" = a + b,
            "-" = a - b,
            "*" = a * b,
            "/" = if(b != 0) a / b else stop("Division by zero"),
            "^" = a ^ b,
            stop("Unknown operator:", token)
          )
          
          stack$push(result)
        }
      }
      
      if (stack$size() != 1) {
        stop("Invalid expression: too many operands")
      }
      
      return(stack$pop())
    },
    
    evaluate_infix = function(expression) {
      "Evaluate infix expression by converting to postfix first"
      postfix <- .self$infix_to_postfix(expression)
      cat("Infix:", expression, "\n")
      cat("Postfix:", postfix, "\n")
      result <- .self$evaluate_postfix(postfix)
      cat("Result:", result, "\n")
      return(result)
    }
  )
)

# Balanced Parentheses Checker
check_balanced_parentheses <- function(expression) {
  "Check if parentheses are balanced in an expression"
  stack <- Stack$new()
  opening <- c("(", "[", "{")
  closing <- c(")", "]", "}")
  pairs <- list(")" = "(", "]" = "[", "}" = "{")
  
  chars <- strsplit(expression, "")[[1]]
  
  for (char in chars) {
    if (char %in% opening) {
      stack$push(char)
    } else if (char %in% closing) {
      if (stack$is_empty()) {
        return(FALSE)  # Closing bracket without opening
      }
      
      if (stack$pop() != pairs[[char]]) {
        return(FALSE)  # Mismatched brackets
      }
    }
  }
  
  return(stack$is_empty())  # Should be empty if balanced
}

# Function Call Stack Simulator
FunctionCallStack <- setRefClass("FunctionCallStack",
  fields = list(
    call_stack = "ANY"
  ),
  methods = list(
    initialize = function() {
      .self$call_stack <- Stack$new()
      cat("Function Call Stack initialized\n")
    },
    
    call_function = function(func_name, params = "") {
      "Simulate a function call"
      call_info <- paste0(func_name, "(", params, ")")
      .self$call_stack$push(call_info)
      cat("Called:", call_info, "| Stack depth:", .self$call_stack$size(), "\n")
    },
    
    return_from_function = function() {
      "Simulate returning from a function"
      if (.self$call_stack$is_empty()) {
        cat("No function to return from\n")
        return()
      }
      
      returned_func <- .self$call_stack$pop()
      cat("Returned from:", returned_func, "| Stack depth:", .self$call_stack$size(), "\n")
    },
    
    show_call_stack = function() {
      "Display the current call stack"
      if (.self$call_stack$is_empty()) {
        cat("Call stack is empty\n")
        return()
      }
      
      cat("Current call stack (bottom to top):\n")
      .self$call_stack$display()
    }
  )
)

# Example usage and testing
if (sys.nframe() == 0) {
  # Demonstrate basic stack operations
  demonstrate_stack_operations()
  
  cat("\n\n=== Expression Evaluation Demonstration ===\n")
  evaluator <- ExpressionEvaluator$new()
  
  # Test infix to postfix conversion and evaluation
  cat("\nExpression 1:\n")
  evaluator$evaluate_infix("2 + 3 * 4")
  
  cat("\nExpression 2:\n")
  evaluator$evaluate_infix("(2 + 3) * 4")
  
  cat("\nExpression 3:\n")
  evaluator$evaluate_infix("2 ^ 3 + 1")
  
  cat("\n\n=== Balanced Parentheses Check ===\n")
  test_expressions <- c(
    "(())",
    "((()))",
    "()[]{}",
    "([{}])",
    "(()",
    "())",
    "([)]"
  )
  
  for (expr in test_expressions) {
    result <- check_balanced_parentheses(expr)
    cat("'", expr, "' is", ifelse(result, "balanced", "not balanced"), "\n")
  }
  
  cat("\n\n=== Function Call Stack Simulation ===\n")
  call_stack <- FunctionCallStack$new()
  
  call_stack$call_function("main", "")
  call_stack$call_function("foo", "x, y")
  call_stack$call_function("bar", "z")
  call_stack$show_call_stack()
  
  call_stack$return_from_function()
  call_stack$return_from_function()
  call_stack$show_call_stack()
  
  call_stack$return_from_function()
  call_stack$show_call_stack()
}