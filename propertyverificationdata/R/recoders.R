
#' Recode variables for models and plots.
#' @export
recode_property_verification_data <- function(frame) {
  frame %>%
    recode_mask_type %>%
    recode_feat_type %>%
    recode_trial_type %>%
    recode_correct_response %>%
    recode_exp %>%
    recode_exp_run
}

#' Recode mask type variable
#' @export
recode_mask_type <- function(frame) {
  mask_type_map <- dplyr::data_frame(
    mask_type = c("nomask", "mask"),
    mask_c = c(-0.5, 0.5),
    mask_f = factor(c("nomask", "mask"), levels = c("nomask", "mask")))
  try(frame <- dplyr::left_join(frame, mask_type_map))
  frame
}


#' Recode feature type variable.
#' @export
recode_feat_type <- function(frame) {
  feat_type_map <- dplyr::data_frame(
    feat_type = c("nonvisual", "visual"),
    feat_c = c(-0.5, 0.5),
    feat_label = c("Encyclopedic Knowledge", "Visual Knowledge"))
  try(frame <- dplyr::left_join(frame, feat_type_map))
  frame
}

#' Recode combination of feature and mask trial type.
#' @export
recode_trial_type <- function(frame) {
  trial_type_map <- dplyr::data_frame(
    feat_type = rep(c("nonvisual", "visual"), times = 2),
    mask_type = rep(c("nomask", "mask"), each = 2),
    trial_type = paste(feat_type, mask_type, sep = ":")
  )
  try(frame <- dplyr::left_join(frame, trial_type_map))
  frame
}

#' Recode experiment run variable.
recode_exp_run <- function(frame) {
  exp_run_map <- dplyr::data_frame(
    exp_run = c(1, 2, 3),
    exp_run_label = c("First run", "Second run", "Third run")
  )
  try(frame <- dplyr::left_join(frame, exp_run_map))
  frame
}

#' Recode the truth of the proposition.
#' @export
recode_correct_response <- function(frame) {
  correct_response_map <- dplyr::data_frame(
    correct_response = c("no", "yes"),
    correct_response_c = c(-0.5, 0.5)
  )
  try(frame <- dplyr::left_join(frame, correct_response_map))
  frame
}

#' Recode experiment name variable
recode_exp <- function(frame) {
  experiment_map <- dplyr::data_frame(
    exp = c("cue_first", "question_first"),
    exp_c = c(-0.5, 0.5))
  try(frame <- dplyr::left_join(frame, experiment_map))
  frame
}
