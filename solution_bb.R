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

bb.list <- function(text) {
  structure(lapply(text, bb), class = c("bb","list"))
}


bb.factor <- function(text) {
  old_class <- class(text)

  character_text <- as.character(text)
  unique_character <- unique(character_text)

  structure(
    factor(bb(character_text), levels = bb(sort(unique_character))),
    class = c("bb", old_class)
  )
}
