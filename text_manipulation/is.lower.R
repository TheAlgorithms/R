is.lowercase <- function(string) {
  # split the string at character level
  string_split <- c(unlist(strsplit(string, split = "")))
  # check if the split string exactly matches its lowercase version
  check_case <- string_split == tolower(string_split)
  # return a boolean value based on the outcome of the check
  if (all(check_case) == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is.lowercase("social")


