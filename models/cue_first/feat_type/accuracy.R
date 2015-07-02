library(lme4)
source("scripts/cue_first_data.R")
source("scripts/contrasts.R")

cue_first <- recode_mask_type(cue_first)
cue_first <- recode_feat_type(cue_first)


# Feature type (visual or nonvisual)
# ----------------------------------
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = cue_first)
summary(feat_type_error_mod)


# Simple effect (visual question only)
# ------------------------------------
visual_type_error_mod <- glmer(is_error ~ mask_c + (1|subj_id), family = binomial,
                               data = filter(cue_first, feat_type == "visual"))
summary(visual_type_error_mod)
