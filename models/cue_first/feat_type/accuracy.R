library(lme4)
source("scripts/cue_first_data.R")
source("scripts/contrasts.R")

# Remove outlier subject
# ----------------------
# Error rate is 65%!!!
cue_first <- filter(cue_first, subj_id != "MWPF129")


# Create contrast variables
# -------------------------
cue_first <- recode_mask_type(cue_first)
cue_first <- recode_feat_type(cue_first)


# Feature type (visual or nonvisual)
# ----------------------------------
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = cue_first)
summary(feat_type_error_mod)


# Simple effect (visual question only)
# ------------------------------------
cue_first_visual_only <- filter(cue_first, feat_type == "visual")
visual_type_error_mod <- glmer(is_error ~ mask_c + (1|subj_id),
                               family = binomial, data = cue_first_visual_only)
summary(visual_type_error_mod)


# Simple effect (nonvisual questions only)
# ----------------------------------------
cue_first_nonvisual_only <- filter(cue_first, feat_type == "nonvisual")
nonvisual_type_error_mod <- glmer(is_error ~ mask_c + (1|subj_id),
                                  family = binomial, data = cue_first_nonvisual_only)
summary(nonvisual_type_error_mod)
