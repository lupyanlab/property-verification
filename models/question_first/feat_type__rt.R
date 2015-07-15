library(lme4)
source("scripts/question_first_data.R")

source("scripts/contrasts.R")

# Create contrast variables
# -------------------------
question_first <- recode_mask_type(question_first)
question_first <- recode_feat_type(question_first)

# Drop outlier subject
# --------------------
question_first <- filter(question_first, subj_id != "MWPF129")

# Predict reaction times from mask_type and cue_type
# --------------------------------------------------
rt_mod <- lmer(rt ~ mask_c * feat_c + (1|subj_id), data = question_first)
summary(rt_mod)
