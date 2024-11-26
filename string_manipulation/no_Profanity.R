no_Profanity <- function(text, block) {
  text_split <- c(unlist(strsplit(text, split = " ")))

  post_n <- c()
  for (i in text_split) {
    post_n <- c(
      post_n,
      if (i %in% c(
        "Damn",
        "damn",
        "DAMN",
        "SHIT",
        "Shit",
        "shit",
        "fuck",
        "Fuck",
        "FUCK",
        "crap",
        "CRAP",
        "Crap",
        "bullshit",
        "Bullshit",
        "BULLSHIT",
        "BullShit",
        "Fucking",
        "fucking",
        "FUCKING"
      )) {
        tolower(i)
      } else {
        i
      }
    )
  }

  clean_text <- gsub("\\b(fuck|shit|bullshit|damn|crap|fucking)\\b", block, post_n)

  clean_text <- gsub("\n", "", clean_text)

  return(paste(clean_text, collapse = " "))
}

post <- "The damn business does not give a crap about it."

no_Profanity(text = post,block = "$$$")
