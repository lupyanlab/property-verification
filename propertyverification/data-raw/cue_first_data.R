devtools::load_all()

cue_first <- compile("data-raw/cue_first/data/", key = "MWPF1",
                     headername = "_header.txt")

# Rename response columns
# -----------------------
cue_first <- rename(cue_first, truth_coded = response, response = response.1)

# Merge norming ratings
# ---------------------
norms <- read.csv("data-raw/norms/feature-norms/feature_norms_merged.csv")
cue_first <- merge(cue_first, norms, all.x = TRUE)

# Merge sensory ratings
# ---------------------
senses <- read.csv("data-raw/norms/feature-norms-senses/feature_norms_senses.csv")
senses <- select(senses, cue, ftype, qid, question, truth_coded, senses_mean)
cue_first <- merge(cue_first, senses, all.x = TRUE)

# Remove practice trials
# ----------------------
cue_first <- filter(cue_first, block_ix != -1)

# Exclude RTs on incorrect responses and timeout trials
# -----------------------------------------------------
cue_first$rt <- with(cue_first, ifelse(is_correct == 0, NA, rt))

# Exclude accuracy on timeout trials
# ----------------------------------
cue_first$is_correct <- with(cue_first, ifelse(response == "timeout", NA, is_correct))

# Make a new column to code accuracy in terms of error
# ----------------------------------------------------
cue_first$is_error <- with(cue_first, ifelse(is_correct == 0, 1, 0))

# Create a unique question identifier
# -----------------------------------
cue_first$question_id <- with(cue_first, paste(cue, ftype, truth_coded, qid, sep = ":"))

# Put the columns in the correct order
# ------------------------------------
cue_first <- cue_first %>%
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

devtools::use_data(cue_first)
