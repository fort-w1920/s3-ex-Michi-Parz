new_bb <- function(text) {
  # replace ((consonants) (1-2 vowels) (consonants)) with
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  bb <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)

  structure(bb, class = "bb")
}


bb <- function(object, ...) UseMethod("bb")

bb.default <- function(text) {
  if (!checkmate::test_character(text)) {
    stop(bb("non-character objects cannot be turned into bb-objects!"))
  }
  new_bb(text)
}

bb.list <- function(list) {
  structure(lapply(list, bb), class = "bb")
}

# DES NOCH ÜBERPRÜFEN: Geht des auch für zweimal den selben factor
# uuuund factor und ordered kann man zusammenfassen.
bb.factor <- function(factor) {
  char <- as.character(factor)
  structure(
    factor(bb(char), levels = bb(sort(char))),
    class = c("bb", "factor")
  )
}

bb.ordered <- function(ordered) {
  char <- as.character(ordered)
  structure(
    ordered(bb(char), levels = bb(sort(char))),
    class = c("bb", "ordered")
  )
}
