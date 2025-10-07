# Ask for user input
input.string <- readline(prompt = "Enter a string: ")

# Convert to lowercase and remove non-letter characters
clean.string <- tolower(gsub("[^a-z]", "", input.string))

# Split string into individual letters
letters.vec <- strsplit(clean.string, "")[[1]]

# Get unique letters
unique.letters <- unique(letters.vec)

# Count occurrences of each unique letter
letter.counts <- table(letters.vec)

# Display results
cat("Unique letters and their counts:\n")
for (letter in unique.letters) {
  cat(letter, ":", letter.counts[letter], "\n")
}
