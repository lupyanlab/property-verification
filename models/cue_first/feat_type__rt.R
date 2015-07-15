library(lme4)
source("scripts/cue_first_data.R")

source("scripts/contrasts.R")

# Create contrast variables
# -------------------------
cue_first <- recode_mask_type(cue_first)
cue_first <- recode_feat_type(cue_first)

# Drop outlier subject
# --------------------
cue_first <- filter(cue_first, subj_id != "MWPF129")

# Predict reaction times from mask_type and cue_type
# --------------------------------------------------
rt_mod <- lmer(rt ~ mask_c * feat_c + (1|subj_id), data = cue_first)
summary(rt_mod)
