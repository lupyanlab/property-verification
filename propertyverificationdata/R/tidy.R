#' @import dplyr
#' @export
tidy_property_verification_data <- function(frame) {
  frame %>%
    # drop practice trials
    filter(block_type != "practice") %>%
    mutate(
      # only measure errors of commission
      is_correct = ifelse(response == "timeout", NA, is_correct),

      raw_rt = rt,
      # only measure rt on correct responses
      rt = ifelse(is_correct == 1, rt, NA),

      # invert correctness to get error
      is_error = 1 - is_correct
    ) %>%
    # put the columns in the correct order
    dplyr::select(
      subj_id, computer, exp_run, seed,
      block, trial,
      question, cue, proposition_id,
      mask_type, feat_type,
      correct_response,
      response, raw_rt, rt, is_correct, is_error) %>%
    # sort the trials
    dplyr::arrange(exp_run, subj_id, block, trial)
}
