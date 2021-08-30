is_anagram <- function(word, target, subject_letters) {
  word_letters <- tolower(strsplit(word, "")[[1]])
  (identical(table(word_letters), target) & !identical(word_letters, subject_letters)) 
}


anagram <- function(subject, candidates) {
  subject_letters <- tolower(strsplit(subject, "")[[1]])
  word_letters <- subject_letters
  target <- table(word_letters)
    
  matches <- sapply(candidates, target, subject_letters, FUN = is_anagram)
  
  if(!any(matches)) {
    c()
  }
  else {
    candidates[matches]
  }

}
