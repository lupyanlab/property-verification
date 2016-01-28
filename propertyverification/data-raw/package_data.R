library(stringr)
library(dplyr)
library(devtools)

load_all()


compile_question_first <- function() {
  first_run <- compile("data-raw/question_first/first_run/", regex_key = "MWPF",
                       header_file = "_header.txt")
  first_run$exp_run <- 1

  second_run <- compile("data-raw/question_first/second_run/", regex_key = "MWPF",
                        header_file = "_header.txt")
  second_run$exp_run <- 2

  question_first <- rbind(first_run, second_run) %>%
    rename_old_experiment_vars

  question_first
}

compile_cue_first <- function() {
  cue_first <- compile("data-raw/cue_first/data/", regex_key = "MWPF1",
                       header_file = "_header.txt")

  cue_first <- cue_first %>%
    mutate(exp_run = 1) %>%
    rename_old_experiment_vars

  cue_first
}

compile_norms <- function() {
  norms <- read.csv("data-raw/norms/norms.csv")
  norms
}

rename_old_experiment_vars <- function(frame) {
  # The old experiment did not include a column with a unique identifier
  # for each proposition (proposition_id).
  frame$question_slug <- frame$question %>%
    str_to_lower() %>%
    str_replace_all(" ", "-") %>%
    str_replace("\\?", "")
  frame$proposition_id <- with(frame, paste(question_slug, cue, sep = ":"))

  frame$block_type <- ifelse(frame$block_ix < 0, "practice", "test")

  frame <- frame %>% rename(
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
    trial = trial_ix,

    computer = room
  ) %>% select(
    # Drop these columns
    -qtype,
    -exp_name,
    -qid,
    -exp_timer,
    -question_slug
  )

  frame
}


cue_first <- compile_cue_first()
question_first <- compile_question_first()
norms <- compile_norms()

cue_first$exp <- "cue_first"
question_first$exp <- "question_first"
property_verification <- rbind(cue_first, question_first)

use_data(cue_first, question_first, norms, property_verification,
         overwrite = TRUE)
