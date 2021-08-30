word_count <- function(input) {
  as.list(
    table(
      strsplit(tolower(trimws(input)), "\\W+")
    )
  ) 
}
