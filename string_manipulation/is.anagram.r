is.anagram <- function(word1, word2) {
  # Convert words to lowercase
  word1 <- tolower(word1)
  word2 <- tolower(word2)
  
  # Check if the words have the same length
  if (nchar(word1) != nchar(word2)) {
    return(FALSE)
  }
  
  # Check if the sorted characters of the words are the same
  sorted_word1 <- sort(strsplit(word1, "")[[1]])
  sorted_word2 <- sort(strsplit(word2, "")[[1]])
  
  if (identical(sorted_word1, sorted_word2)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is.anagram(word1 = "rats",word2 = "star")
