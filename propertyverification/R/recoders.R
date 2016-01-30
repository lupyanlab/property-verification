
recode_mask_type <- function(frame) {
  mask_type_map <- dplyr::data_frame(
    mask_type = c("nomask", "mask"),
    mask_c = c(-0.5, 0.5),
    mask_f = factor(c("nomask", "mask"), levels = c("nomask", "mask")))
  dplyr::left_join(frame, mask_type_map)
}

recode_feat_type <- function(frame) {
  feat_type_map <- dplyr::data_frame(
    feat_type = c("nonvisual", "visual"),
    feat_c = c(-0.5, 0.5),
    feat_label = c("Encyclopedic Knowledge", "Visual Knowledge"))
  dplyr::left_join(frame, feat_type_map)
}

recode_exp <- function(frame) {
  experiment_map <- dplyr::data_frame(
    exp = c("cue_first", "question_first"),
    exp_c = c(-0.5, 0.5))
  dplyr::left_join(frame, experiment_map)
}

recode_exp_run <- function(frame) {
  exp_run_map <- dplyr::data_frame(
    exp_run = c(1, 2, 3),
    exp_run_label = c("First run", "Second run", "Third run")
  )
  dplyr::left_join(frame, exp_run_map)
}
