options(stringsAsFactors = FALSE)
source("scripts/compile.R")

# check global flag
if (!exists("CLEAR_GLOBAL_ENVIRONMENT")) {
  CLEAR_GLOBAL_ENVIRONMENT <- TRUE
}

question_first <- compile("data/question_first/data/", key = "MWPF2",
                          headername = "_header.txt")

# Rename response columns
# -----------------------
question_first <- rename(question_first, truth_coded = response, response = response.1)

# Merge norming ratings
# ---------------------
norms <- read.csv("norms/feature-norms/feature_norms_merged.csv")
question_first <- merge(question_first, norms, all.x = TRUE)

# Merge sensory ratings
# ---------------------
senses <- read.csv("norms/feature-norms-senses/feature_norms_senses.csv")
senses <- select(senses, cue, ftype, qid, question, truth_coded, senses_mean)
question_first <- merge(question_first, senses, all.x = TRUE)

# Remove practice trials
# ----------------------
question_first <- filter(question_first, block_ix != -1)

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

# Create a unique question identifier
# -----------------------------------
question_first$question_id <- with(question_first, paste(cue, ftype, truth_coded, qid, sep = ":"))

# Put the columns in the correct order
# ------------------------------------
question_first <- question_first %>%
  select(subj_id, block_ix, trial_ix, 
         cue, question, question_id,
         mask_type = cue_mask,
         feat_type = ftype,
         truth_coded,
         imagery_mean, imagery_z,
         facts_mean, facts_z,
         diff_mean = difficulty_mean, diff_z = difficulty_z,
         prop_visual,
         senses_mean,
         response, rt, is_correct, is_error) %>%
  arrange(subj_id, block_ix, trial_ix)

if (CLEAR_GLOBAL_ENVIRONMENT == TRUE) {
  # Remove unneeded variables
  # -------------------------
  rm(list = setdiff(ls(), "question_first"))
}