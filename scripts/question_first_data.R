options(stringsAsFactors = FALSE)
library(dplyr)
source("scripts/compile.R")

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

# Fix miscoded questions
# ----------------------

# start with the original scoring as "truth"
question_first$truth <- question_first$truth_coded

# get a list of the miscoded questions
miscoded <- (!is.na(question_first$truth_agree) &
             question_first$truth_agree == 0 &
             question_first$truth_coded == "no")

# fix the coding to match the norms
question_first[miscoded, "truth"] <- question_first[miscoded, "truth_normed"]

# update is_correct with the new scoring column
question_first$is_correct <- with(question_first, ifelse(response == truth, 1, 0))

# Remove practice trials
# ----------------------
question_first <- filter(question_first, block_ix != -1)

# Exclude RTs on incorrect responses and timeout trials
# -----------------------------------------------------
question_first$rt <- with(question_first, ifelse(is_correct == 0, NA, rt))

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
         truth_coded, truth_normed, truth,
         imagery_mean, imagery_z,
         facts_mean, facts_z,
         diff_mean = difficulty_mean, diff_z = difficulty_z,
         prop_visual,
         senses_mean,
         response, rt, is_correct, is_error) %>%
  arrange(subj_id, block_ix, trial_ix)

# Remove unneeded variables
# -------------------------
rm(list = setdiff(ls(), "question_first"))
