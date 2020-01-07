new_bb <- function(text) {
  checkmate::assert_character(text)
  
  # replace ((consonants) (1-2 vowels) (consonants)) with
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  bb <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
  
  structure(bb, class = "bb")
}


bb <- function(object, ...) UseMethod("bb")

bb.default <- function(text){
  new_bb(text)
}

bb.list <- function(list){
  structure(lapply(list,bb), class = "bb")
}

bb.factor <- function(x){
  char <- as.character(x)
  structure(
    factor(bb(char), levels = bb(sort(char))),
    class = c("bb","factor")
  )
}

bb.ordered <- function(x){
  char <- as.character(x)
  structure(
    ordered(bb(char), levels = bb(sort(char))),
    class = c("bb","ordered")
  )
}
