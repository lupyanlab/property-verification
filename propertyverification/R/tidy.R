tidy_property_verification_data <- function(frame) {
  # Remove practice trials
  # ----------------------
  frame <- filter(frame, block != -1)

  # Exclude RT on timeout trials
  # ----------------------------
  frame$rt <- with(frame, ifelse(response == "timeout", NA, rt))

  # Save raw RTs for investigating speed-accuracy tradeoff
  # ------------------------------------------------------
  frame$raw_rt <- frame$rt

  # Exclude RTs on incorrect responses and timeout trials
  # -----------------------------------------------------
  frame$rt <- with(frame, ifelse(is_correct == 0, NA, rt))

  # Exclude accuracy on timeout trials
  # ----------------------------------
  frame$is_correct <- with(frame, ifelse(response == "timeout", NA, is_correct))

  # Make a new column to code accuracy in terms of error
  # ----------------------------------------------------
  frame$is_error <- with(frame, ifelse(is_correct == 0, 1, 0))

  # Merge norming ratings
  # ---------------------
  data(norms)
  frame <- merge(frame, norms, all.x = TRUE)

  # Put the columns in the correct order
  # ------------------------------------
  frame <- frame %>%
    select(subj_id, exp_run,
           block, trial,
           cue, question, proposition_id,
           mask_type, feat_type,
           correct_response,
           imagery_mean, imagery_z,
           facts_mean, facts_z,
           difficulty_mean, difficulty_z,
           prop_visual,
           senses_mean, senses_z,
           response, rt, is_correct, is_error,
           raw_rt) %>%
    arrange(exp_run, subj_id, block, trial)

  frame
}
