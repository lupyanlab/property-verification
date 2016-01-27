library(stringr)
library(dplyr)
library(devtools)

load_all()

compile_question_first <- function(overwrite = FALSE) {
  first_run <- compile("data-raw/question_first/first_run/", regex_key = "MWPF",
                       header_file = "_header.txt")
  first_run$exp_run <- 1

  second_run <- compile("data-raw/question_first/second_run/", regex_key = "MWPF",
                        header_file = "_header.txt")
  second_run$exp_run <- 2

  question_first <- rbind(first_run, second_run) %>%
    rename_old_exp_vars %>%
    tidy_property_verification_data

    use_data(question_first, overwrite = overwrite)
  question_first
}

compile_cue_first <- function(overwrite = FALSE) {
  cue_first <- compile("data-raw/cue_first/data/", regex_key = "MWPF1",
                       header_file = "_header.txt")

  cue_first <- cue_first %>%
    mutate(exp_run = 1) %>%
    rename_old_exp_vars %>%
    tidy_property_verification_data

  use_data(cue_first, overwrite = overwrite)
  cue_first
}

compile_property_verification <- function(overwrite = FALSE) {
  cue_first <- compile_cue_first(overwrite = overwrite)
  cue_first$exp <- "cue_first"

  question_first <- compile_question_first(overwrite = overwrite)
  question_first$exp <- "question_first"

  property_verification <- rbind_list(cue_first, question_first)
  use_data(property_verification, overwrite = overwrite)
}

rename_old_experiment_vars <- function(frame) {
  frame <- rename(frame,
                  # There were two columns named response in the original
                  # experiment. The first response column, generated when the
                  # trials were created, corresponds to the correct_response.
                  # The second response column is the participant's response on
                  # that trial.
                  correct_response = response,
                  response = response.1,
                  
                  # mask_type and feat_type are much better names
                  mask_type = cue_mask,
                  feat_type = ftype,

                  # In later experiments I dropped the "_ix" suffix
                  block = block_ix,
                  trial = trial_ix)

  frame$question_slug <- frame$question %>%
    str_to_lower() %>%
    str_replace_all(" ", "-") %>%
    str_replace("\\?", "")
  frame$proposition_id <- with(frame, paste(question_slug, cue, sep = ":"))

  frame
}

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
  norms <- read.csv("data-raw/norms/norms.csv")
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
