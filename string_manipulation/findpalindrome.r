findPalindrome <- function(input) {
  
  is.palindrome <- function(input) {
    if (is.numeric(input)) { # checks if input is numeric
      # convert the numeric input value to character
      input_con <- as.character(input)
      # split the string into characters
      input_split <- c(unlist(strsplit(input_con, split = "")))
      # reverse the characters
      input_rev <- rev(input_split)
      # conditional statement to compare split string
      # with the reversed string
      if (all(input_split == input_rev) != TRUE) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    } else if (is.character(input)) { # checks if input is character
      # split the string into characters
      input_split <- c(unlist(strsplit(tolower(input), split = "")))
      # reverse the characters
      input_rev <- rev(input_split)
      # conditional statement to compare split string
      # with the reversed string
      if (all(input_split == input_rev) != TRUE) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
  }

  if(is.character(input)) {
    # clean out punctuation
    input_clean <- tm::removePunctuation(input)
    
    # split the sentence into individual words
    input_split <- c(unlist(strsplit(input_clean, split = " ")))
    
    # loop every word in the text through the is.palindrome() function
    # and return their boolean values
    check_palin <- c()
    for (i in input_split) {
      result <- is.palindrome(i)
      check_palin <- append(check_palin, result)
    }
    
    # check and return index positions of TRUE
    indx <- which(check_palin == TRUE)
    
    # use the index positions to filter input_split
    palindromes <- input_split[indx]
    
    # filter out words that contain less than one character
    palin <- palindromes[nchar(palindromes) > 1]
    
    return(noquote(palin))
    
  } else if(is.numeric(input)) {
    # convert numeric input to character
    num_con <- as.character(input)
    
    # clean out punctuation
    input_clean <- tm::removePunctuation(num_con)
    
    # split the sentence into individual words
    input_split <- c(unlist(strsplit(input_clean, split = " ")))
    
    # loop every word in the text through the is.palindrome() function
    # and return their boolean values
    check_palin <- c()
    for (i in input_split) {
      result <- is.palindrome(i)
      check_palin <- append(check_palin, result)
    }
    
    # check and return index positions of TRUE
    indx <- which(check_palin == TRUE)
    
    # use the index positions to filter input_split
    palindromes <- input_split[indx]
    
    # filter out numbers that contain less than one character
    palin <- palindromes[nchar(palindromes) > 1]
    
    return(noquote(palin))
    
  }
}

my_text <- "Bob works in a shop on a street called Pop. His work ID is 444, and his manager owns a racecar."

findPalindrome(my_text)
