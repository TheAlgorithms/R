# String Rotation Check in R
# Checks if one string is a rotation of another string.

is_rotation <- function(s1, s2) {
  # Check length first
  if (nchar(s1) != nchar(s2)) {
    return(FALSE)
  }
  
  # Concatenate s1 with itself and check if s2 is a substring
  combined <- paste0(s1, s1)
  return(grepl(s2, combined, fixed = TRUE))
}

# Interactive input
s1 <- tolower(readline("Enter first string: "))
s2 <- tolower(readline("Enter second string: "))

if (is_rotation(s1, s2)) {
  cat("Yes, '", s2, "' is a rotation of '", s1, "'.\n", sep = "")
} else {
  cat("No, '", s2, "' is not a rotation of '", s1, "'.\n", sep = "")
}