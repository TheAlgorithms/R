#' Check if a number is a palindrome
#'
#' @description Checks if an integer is a palindrome (reads the same forwards
#'   and backward) without using strings. Negative numbers are not palindromes.
#' @param number The integer to check.
#' @return TRUE if the number is a palindrome, FALSE otherwise.
#' @examples
#' isPalindrome(121)
#' isPalindrome(123)

isPalindrome <- function(number) {
  # Negative numbers are not considered palindromes
  if (number < 0L) {
    return(FALSE)
  }

  original_number <- number
  reversed_number <- 0L

  # Loop while the number is greater than 0
  while (number > 0L) {
    # Get the last digit
    remainder <- number %% 10L
    
    # Build the reversed number
    reversed_number <- (reversed_number * 10L) + remainder
    
    # Remove the last digit using integer division
    number <- number %/% 10L
  }

  # Return TRUE if the original and reversed numbers are the same
  return(original_number == reversed_number)
}

isPalindrome(121)
isPalindrome(123)
isPalindrome(7)
isPalindrome(-101)
