parse_phone_number <- function(number_string) {
  numbers <- gsub("\\D", "", number_string)
  # 1(NXX)-NXX-XXXX, N: 2-9, X: 0-9
  telp <- "^(1|)([2-9][0-9]{2}[2-9][0-9]{6})$"
  if (grepl(telp, numbers)) {
    # take the 2nd group
    gsub(telp, "\\2", numbers)
  } else {
    NULL
  }
}

