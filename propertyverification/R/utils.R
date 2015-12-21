
label_duplicates <- function(question_ids) {
  seen <- c()
  is_unique <- c()
  for (q in question_ids) {
    is_unique <- c(is_unique, q %in% seen)
    seen <- c(seen, q)
  }
  is_unique
}
