# Sample Input-1 
# n = 10

# Sample Output-1
# "First 10 Fibonacci numbers:"
# 0  1  1  2  3  5  8 13 21 34



# Sample Input-2
# n = 25

# Sample Output-2
# "First 25 Fibonacci numbers:"
# 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368


# Implementation

# taking input 
n = readline(prompt = "Enter the Number : ");

# convert the inputted value to an integer
n = as.integer(n);

# creating empty array of size n
Fibonacci <- numeric(n)

# assigning first 2 fibonacci values
Fibonacci[1] <- 0
Fibonacci[2] <- 1

# finding the remaining fibonacci numbers using a for loop ranging from 3 to n
for (i in 3:n) Fibonacci[i] <- Fibonacci[i - 2] + Fibonacci[i - 1]

# printing the result
print(paste("First",n,"Fibonacci numbers:"))
print(Fibonacci)
