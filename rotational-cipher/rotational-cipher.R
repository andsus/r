ALPHABET_SIZE <- 26

rotate_chiper <- function(c, key) {
  if (c %in% letters) {
    baseChar <- utf8ToInt("a")
  } else if (c %in% LETTERS) {
    baseChar <- utf8ToInt("A")
  } else {
    return(c)
  }
  intToUtf8(baseChar + (utf8ToInt(c) - baseChar + key) %% ALPHABET_SIZE)
}

rotate <- function(text, key) {
  paste(lapply(unlist(strsplit(text,"")), function(c) rotate_chiper(c, key)) ,collapse="")
}
