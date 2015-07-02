require(dplyr)
require(car)                      # import `recode` function
source("./helper/helper-funcs.R") # import `%nin%` function for filtering

df <- read.csv("./data/mwpf1-2014-04-18.csv", stringsAsFactors = FALSE)

# two response columns: truth coded for feedback, and the subjs response
df <- plyr::rename(df, c("response" = "truth_coded", "response.1" = "response"))

# merge norming ratings
# norms <- read.csv("./feature-norms/feature_norms.csv")
norms <- read.csv("./feature-norms/feature_norms_merged.csv")
df <- merge(df, norms, all.x = T)

# merge sense ratings
senses <- read.csv("./feature-norms-senses/feature_norms_senses.csv")
senses <- select(senses, cue, ftype, qid, question, truth_coded, senses_mean)
df <- merge(df, senses, all.x = T)

# fix the miscoded questions
df$truth <- df$truth_coded             # save the original scoring mechanism in truth

miscoded <- (!is.na(df$truth_agree) &  # select the questions that were miscoded
             df$truth_agree == 0 &     #
             df$truth_coded == "no")   # and only the ones we made
df[miscoded, "truth"] <- df[miscoded, "truth_normed"]

# update is_correct with the new scoring column
df$is_correct <- ifelse(df$response == df$truth, 1, 0)

df <- df %.%
  filter(
    block_ix != -1             # remove practice trials
  ) %.% 
  mutate(
    # exclude RTs on incorrect and timeout trials
    rt = ifelse(is_correct == 0, NA, rt),
    
    # exclude accuracy on timeout trials
    is_correct = ifelse(response == "timeout", NA, is_correct),
    
    # code accuracy in terms of error
    is_error = ifelse(is_correct == 0, 1, 0),
    
    # create a unique question identifier
    question_id = paste(cue, ftype, truth_coded, qid, sep = ":"),
    
    # code variables for linear models
    mask_c = recode(cue_mask, "'nomask'=-0.5; 'mask'=0.5"),
    feat_c = recode(ftype, "'nonvisual'=-0.5; 'visual'=0.5"),
    
    truth_c = recode(truth, "'no' = -0.5; 'yes' = 0.5")
  ) %.%
  select(subj_id, block_ix, trial_ix, 
         cue, question, question_id,
         mask_type = cue_mask, mask_c,
         feat_type = ftype, feat_c,
         truth_coded, truth_normed, truth, truth_c,
         imagery_mean, imagery_z,
         facts_mean, facts_z,
         diff_mean = difficulty_mean, diff_z = difficulty_z,
         prop_visual,
         senses_mean,
         response, rt, is_correct, is_error) %.%
  arrange(subj_id, block_ix, trial_ix)

write.csv(df, file = "./feature-first/feature-first-senses.csv", row.names = FALSE)
