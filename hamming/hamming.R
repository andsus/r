# This is a stub function to take two strings
# and calculate the hamming distance
hamming <- function(strand1, strand2) {
  if (nchar(strand1) != nchar(strand2)) {
    stop("strands have unequal length")
  }
  strands <- strsplit(c(strand1, strand2), "")
  sum(strands[[1]] != strands[[2]])
}
