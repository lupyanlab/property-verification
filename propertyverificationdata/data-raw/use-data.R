# Compile all raw .csv and .txt files from the various experiments
# and save them to the package as .rda files.

library(stringr)
library(dplyr)

options(stringsAsFactors = FALSE)

# Load the compile function.
# Don't load the package because that would load the data that is being compiled.
source("R/compile.R")

compile_question_first <- function() {
  first_run <- compile("data-raw/question_first/first_run/data/", regex_key = "MWPF",
                       header_file = "_header.txt")
  first_run$exp_run <- 1

  second_run <- compile("data-raw/question_first/second_run/data/", regex_key = "MWPF",
                        header_file = "_header.txt")
  second_run$exp_run <- 2

  question_first <- rbind(first_run, second_run) %>%
    rename_old_experiment_vars

  third_run <- compile("data-raw/question_first/third_run/data/", regex_key = "MWPR") %>%
    mutate(exp_run = 3)

  fourth_run <- compile("data-raw/question_first/fourth_run/data/", regex_key = "PV") %>%
    mutate(exp_run = 4) %>%
    select(-experimenter)

  # Drop participants in George that were under the incorrect monitor conditions
  wrong_conditions <- filter(fourth_run, computer == "LL-George", seed < 137) %>%
    .$subj_id %>% unique
  fourth_run <- filter(fourth_run, !(subj_id %in% wrong_conditions))

  # Drop participant that was snapchatting
  fourth_run <- filter(fourth_run, subj_id != "PV123")

  question_first <- rbind_list(question_first, third_run, fourth_run)

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

compile_norms_responses <- function() {
  norms_responses <- read.csv("data-raw/norms/norms_responses.csv")
  norms_responses
}

compile_subj_info <- function() {
  subj_info_files <- list.files("data-raw", pattern = "subj_info.csv",
                                full.names = TRUE, recursive = TRUE)
  subj_info <- plyr::ldply(subj_info_files, readr::read_csv)
}

compile_survey <- function() {
  qualtrics_survey_files <- list.files("data-raw", pattern = "qualtrics_survey.csv",
                                       full.names = TRUE, recursive = TRUE)
  qualtrics_surveys <- plyr::ldply(qualtrics_survey_files, readr::read_csv) %>%
    process_qualtrics_survey

  google_survey_files <- list.files("data-raw", pattern = "google_survey.csv",
                                    full.names = TRUE, recursive = TRUE)
  google_surveys <- plyr::ldply(google_survey_files, readr::read_csv) %>%
    process_google_survey

  rbind_list(qualtrics_surveys, google_surveys)
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

process_qualtrics_survey <- function(frame) {
  frame %>%
    select(subj_id, computer = room, strategy)
}

process_google_survey <- function(frame) {
  frame %>%
    select(subj_id, computer, strategy = contains("strategy"))
}

cue_first <- compile_cue_first()
question_first <- compile_question_first()

norms <- compile_norms()
norms_responses <- compile_norms_responses()

subj_info <- compile_subj_info()
survey <- compile_survey()
coded_strategies <- read.csv("data-raw/question_first/coded_strategies.csv",
                             na.strings = "")

cue_first$exp <- "cue_first"
question_first$exp <- "question_first"
property_verification <- rbind(cue_first, question_first)

# Individual diffs
individual_diffs <- read.csv("data-raw/individual_diffs/imagery.csv")

devtools::use_data(property_verification, cue_first, question_first,
                   norms, norms_responses,
                   subj_info, survey, coded_strategies,
                   individual_diffs,
                   overwrite = TRUE)
