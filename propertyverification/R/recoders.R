
recode_mask_type <- function(frame) {
  mask_type_map <- data.frame(
    mask_type = c("nomask", "mask"),
    mask_c = c(-0.5, 0.5),
    mask_f = factor(c("nomask", "mask"), levels = c("nomask", "mask")))
  merge(frame, mask_type_map, all.x = TRUE)
}

recode_feat_type <- function(frame) {
  feat_type_map <- data.frame(
    feat_type = c("nonvisual", "visual"),
    feat_c = c(-0.5, 0.5),
    feat_label = c("Encyclopedic Knowledge", "Visual Knowledge"))
  merge(frame, feat_type_map, all.x = TRUE)
}

recode_exp <- function(frame) {
  experiment_map <- data.frame(
    exp = c("cue_first", "question_first"),
    exp_c = c(-0.5, 0.5))
  merge(frame, experiment_map, all.x = TRUE)
}

recode_exp_run <- function(frame) {
  exp_run_map <- data_frame(
    exp_run = c(1, 2, 3),
    exp_run_label = c("First run", "Second run", "Third run")
  )
  frame %>% left_join(exp_run_map)
}