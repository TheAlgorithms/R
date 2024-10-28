is.uppercase <- function(string) {
  # split the string at character level
  string_split <- c(unlist(strsplit(string, split = "")))
  # check if the split string exactly matches its uppercase version
  check_case <- string_split == toupper(string_split)
  # return a boolean value based on the outcome of the check
  return(all(check_case))
}

is.uppercase("BUSINESS")
