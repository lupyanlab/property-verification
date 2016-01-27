library(stringr)
library(dplyr)
library(devtools)

# load_all()

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

compile_norms <- function(overwrite = FALSE) {
  norms <- read.csv("data-raw/norms/norms.csv")
  use_data(norms, overwrite = overwrite)
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
