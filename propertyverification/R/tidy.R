#' @importFrom magrittr %>%
#' @export
tidy_property_verification_data <- function(frame) {
  # Remove practice trials
  frame <- dplyr::filter(frame, block_type != "practice")

  # Exclude RT on timeout trials
  frame$rt <- with(frame, ifelse(response == "timeout", NA, rt))

  # Save raw RTs for investigating speed-accuracy tradeoff
  frame$raw_rt <- frame$rt

  # Exclude RTs on incorrect responses and timeout trials
  frame$rt <- with(frame, ifelse(is_correct == 0, NA, rt))

  # Exclude accuracy on timeout trials
  frame$is_correct <- with(frame, ifelse(response == "timeout", NA, is_correct))

  # Make a new column to code accuracy in terms of error
  frame$is_error <- with(frame, ifelse(is_correct == 0, 1, 0))

  # Put the columns in the correct order
  frame <- frame %>%
    dplyr::select(subj_id, exp_run,
           block, trial,
           question, cue, proposition_id,
           mask_type, feat_type,
           correct_response,
           response, rt, is_correct, is_error,
           raw_rt) %>%
    dplyr::arrange(exp_run, subj_id, block, trial)

  frame
}
