#' Label the order that questions were exposed to participants.
#'
#' @import dplyr
#' @export
label_exposure_order <- function(frame) {
  frame %>%
    group_by(subj_id, question) %>%
    mutate(exposure_order = seq_along(question))
}

#' Label the first appearance of unique values in a vector.
#'
#' @export
label_duplicates <- function(question_ids) {
  seen <- c()
  is_unique <- c()
  for (q in question_ids) {
    is_unique <- c(is_unique, q %in% seen)
    seen <- c(seen, q)
  }
  is_unique
}
