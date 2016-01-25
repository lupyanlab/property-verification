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

  question_first <- rbind(first_run, second_run)

  # Rename response columns
  # -----------------------
  # There were two columns named response in the original experiment.
  # The first response column, generated when the trials were created,
  # corresponds to the correct_response. The second response column is
  # the participant's response on that trial.
  question_first <- rename(question_first,
                           correct_response = response,
                           response = response.1)

  # Remove practice trials
  # ----------------------
  question_first <- filter(question_first, block_ix != -1)

  # Exclude RT on timeout trials
  # ----------------------------
  question_first$rt <- with(question_first, ifelse(response == "timeout", NA, rt))

  # Save raw RTs for investigating speed-accuracy tradeoff
  # ------------------------------------------------------
  question_first$raw_rt <- question_first$rt

  # Exclude RTs on incorrect responses and timeout trials
  # -----------------------------------------------------
  question_first$rt <- with(question_first, ifelse(is_correct == 0, NA, rt))

  # # Correct the calculation of RT
  # # -----------------------------
  # # The RT timer didn't start until after the offset of the cue,
  # # but really it should have started at the onset of the cue.
  # # In order to correct for this coding error, the duration of each
  # # cue file needs to be added to the measured RT.
  # cue_durations <- read.csv("experiment/stimuli/cues/_cue_stats.csv")
  # cue_durations$cue_dur <- cue_durations$cue_dur * 1000  # convert sec to ms
  # question_first <- merge(question_first, cue_durations, all.x = TRUE)
  # question_first$rt <- with(question_first, cue_dur + rt)

  # On second thought, people were prevented from answering before the
  # prompt, and some of the cues were pretty long (~800 msec) so the
  # above is not truly a correct calculation of RT.

  # Exclude accuracy on timeout trials
  # ----------------------------------
  question_first$is_correct <- with(question_first, ifelse(response == "timeout", NA, is_correct))

  # Make a new column to code accuracy in terms of error
  # ----------------------------------------------------
  question_first$is_error <- with(question_first, ifelse(is_correct == 0, 1, 0))

  # Create a unique proposition identifier
  # -----------------------------------
  question_first$question_slug <- question_first$question %>%
    str_to_lower %>%
    str_replace_all(" ", "-") %>%
    str_replace("\\?", "")
  question_first$proposition_id <- with(question_first, paste(question_slug, cue, sep = ":"))

  # Merge norming ratings
  # ---------------------
  norms <- read.csv("data-raw/norms/norms.csv")
  question_first <- merge(question_first, norms, all.x = TRUE)

  # Put the columns in the correct order
  # ------------------------------------
  question_first <- question_first %>%
    select(subj_id, exp_run,
           block = block_ix, trial = trial_ix,
           cue, question, proposition_id,
           mask_type = cue_mask,
           feat_type = ftype,
           correct_response,
           imagery_mean, imagery_z,
           facts_mean, facts_z,
           difficulty_mean, difficulty_z,
           prop_visual,
           senses_mean, senses_z,
           response, rt, is_correct, is_error,
           raw_rt) %>%
    arrange(exp_run, subj_id, block, trial)

  use_data(question_first, overwrite = overwrite)
}
