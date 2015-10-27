
recode_mask_type <- function(frame) {
  mask_type_map <- data.frame(
    mask_type = c("nomask", "mask"),
    mask_c = c(-0.5, 0.5))
  merge(frame, mask_type_map, all.x = TRUE)
}

recode_feat_type <- function(frame) {
  feat_type_map <- data.frame(
    feat_type = c("nonvisual", "visual"),
    feat_c = c(-0.5, 0.5))
  merge(frame, feat_type_map, all.x = TRUE)
}

recode_exp <- function(frame) {
  experiment_map <- data.frame(
    exp = c("cue_first", "question_first"),
    exp_c = c(-0.5, 0.5))
  merge(frame, experiment_map, all.x = TRUE)
}