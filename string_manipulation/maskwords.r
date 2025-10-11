maskWords <- function(text, mask) {
  text_split <- c(unlist(strsplit(text, split = " ")))

  post_n <- c()
  for (i in text_split) {
    post_n <- c(
      post_n,
      if (i %in% c(
        "birds",
        "BIRDS",
        "Birds",
        "market",
        "Market",
        "MARKET",
        "street",
        "STREET",
        "Street"
      )) {
        tolower(i)
      } else {
        i
      }
    )
  }

  clean_text <- gsub("\\b(birds|street|market)\\b", mask, post_n)

  clean_text <- gsub("\n", "", clean_text)

  return(paste(clean_text, collapse = " "))
}

post <- "The lady bought groceries from the market, but some of them spilled on the street, and the birds helped themselves."

maskWords(text = post,mask = "$$$")
