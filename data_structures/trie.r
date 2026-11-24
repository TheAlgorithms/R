# A trie (prefix tree) stores strings so that words sharing a prefix share
# the same path from the root. Each node holds one child per character and a
# flag marking the end of a word.
#
# Time Complexities (m = length of the word or prefix):
# - Insert:         O(m)
# - Search:         O(m)
# - Prefix check:   O(m)
# - Delete:         O(m)
# - Autocomplete:   O(m + k) where k = number of characters in the results
#
# Space Complexity: O(total number of characters stored)
#
# Applications:
# - Autocomplete and spell checking
# - Dictionary and prefix lookups
# - IP routing (longest prefix match)
# - Word games (Boggle, Scrabble solvers)
#
# Example usage:
# t <- trie_new()
# trie_insert(t, "apple")
# trie_search(t, "apple")           # TRUE
# trie_starts_with(t, "app")        # TRUE
# trie_autocomplete(t, "app")       # "apple"
# trie_delete(t, "apple")
#
# Note: the trie is built from R environments, which have reference
# semantics, so trie_insert() and trie_delete() modify the trie in place.

trie_new_node <- function() {
  node <- new.env(hash = TRUE, parent = emptyenv())
  node$children <- new.env(hash = TRUE, parent = emptyenv())
  node$is_end <- FALSE
  node
}

#' Create an empty trie
#' @return A trie (root node)
trie_new <- function() {
  trie_new_node()
}

# Split a word into characters; empty string gives character(0)
trie_chars <- function(word) {
  if (!is.character(word) || length(word) != 1 || is.na(word)) {
    stop("Error: word must be a single string")
  }
  strsplit(word, "", fixed = TRUE)[[1]]
}

# Walk the trie along a string; return the final node or NULL if missing
trie_find_node <- function(trie, text) {
  node <- trie
  for (ch in trie_chars(text)) {
    if (!exists(ch, envir = node$children, inherits = FALSE)) {
      return(NULL)
    }
    node <- get(ch, envir = node$children, inherits = FALSE)
  }
  node
}

#' Insert a word into the trie
#' @param trie Trie created by trie_new()
#' @param word Single string to insert
#' @return The trie, invisibly (modified in place)
trie_insert <- function(trie, word) {
  node <- trie
  for (ch in trie_chars(word)) {
    if (!exists(ch, envir = node$children, inherits = FALSE)) {
      assign(ch, trie_new_node(), envir = node$children)
    }
    node <- get(ch, envir = node$children, inherits = FALSE)
  }
  node$is_end <- TRUE
  invisible(trie)
}

#' Check whether a whole word is stored in the trie
#' @return TRUE or FALSE
trie_search <- function(trie, word) {
  node <- trie_find_node(trie, word)
  !is.null(node) && node$is_end
}

#' Check whether any stored word starts with the prefix
#' @return TRUE or FALSE
trie_starts_with <- function(trie, prefix) {
  !is.null(trie_find_node(trie, prefix))
}

#' Delete a word from the trie, pruning nodes no longer needed
#' @return TRUE if the word was removed, FALSE if it was not present
trie_delete <- function(trie, word) {
  if (!trie_search(trie, word)) {
    return(FALSE)
  }
  chars <- trie_chars(word)
  # Record the path so unused nodes can be pruned bottom-up
  path <- list(trie)
  node <- trie
  for (ch in chars) {
    node <- get(ch, envir = node$children, inherits = FALSE)
    path[[length(path) + 1]] <- node
  }
  node$is_end <- FALSE
  for (i in rev(seq_along(chars))) {
    current <- path[[i + 1]]
    if (current$is_end || length(ls(current$children, all.names = TRUE)) > 0) {
      break
    }
    rm(list = chars[i], envir = path[[i]]$children)
  }
  TRUE
}

# Collect every word stored below a node. Characters of the current path are
# kept in one shared vector and joined only when a word ends, so no growing
# prefix string is rebuilt at every node.
trie_collect <- function(node, prefix) {
  words <- list()
  path <- character(0)
  walk <- function(current, depth) {
    if (current$is_end) {
      words[[length(words) + 1L]] <<- paste0(
        prefix, paste0(path[seq_len(depth)], collapse = "")
      )
    }
    for (ch in sort(ls(current$children, all.names = TRUE))) {
      path[depth + 1L] <<- ch
      walk(get(ch, envir = current$children, inherits = FALSE), depth + 1L)
    }
  }
  walk(node, 0L)
  as.character(unlist(words))
}

#' List all stored words starting with the prefix, in sorted order
#' @return Character vector (empty if there are no matches)
trie_autocomplete <- function(trie, prefix) {
  node <- trie_find_node(trie, prefix)
  if (is.null(node)) {
    return(character(0))
  }
  trie_collect(node, prefix)
}

# ===========================
# Example Usage & Testing
# ===========================
t <- trie_new()
for (w in c("apple", "app", "apply", "apt", "bat", "bath")) {
  trie_insert(t, w)
}

stopifnot(trie_search(t, "apple"))
stopifnot(trie_search(t, "app"))
stopifnot(!trie_search(t, "ap"))
stopifnot(!trie_search(t, "bats"))
stopifnot(trie_starts_with(t, "ba"))
stopifnot(!trie_starts_with(t, "cat"))
stopifnot(identical(trie_autocomplete(t, "app"), c("app", "apple", "apply")))
stopifnot(identical(trie_autocomplete(t, "ba"), c("bat", "bath")))
stopifnot(length(trie_autocomplete(t, "z")) == 0)

# Deleting a word keeps other words that share its prefix
stopifnot(trie_delete(t, "app"))
stopifnot(!trie_search(t, "app"))
stopifnot(trie_search(t, "apple"))
stopifnot(!trie_delete(t, "app"))

# Deleting a leaf prunes its unused nodes
stopifnot(trie_delete(t, "bath"))
stopifnot(trie_search(t, "bat"))
stopifnot(!trie_starts_with(t, "bath"))

cat("Autocomplete 'ap':", paste(trie_autocomplete(t, "ap"), collapse = ", "), "\n")
cat("All trie tests passed\n")
