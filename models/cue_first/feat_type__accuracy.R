source("scripts/cue_first_data.R")
library(lme4)
source("scripts/contrasts.R")

# Remove outlier subject
# ----------------------
# Error rate is 65%!!!
source("scripts/outliers.R")
cue_first <- filter(cue_first, subj_id != cue_first_outliers)

# Create contrast variables
# -------------------------
cue_first <- recode_mask_type(cue_first)
cue_first <- recode_feat_type(cue_first)

# Feature type (visual or nonvisual)
# ----------------------------------
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = cue_first)
summary(feat_type_error_mod)

# Feature type (nomask only)
cue_first_nomask <- filter(cue_first, mask_type == "nomask")
feat_type_error_mod_nomask <- glmer(is_error ~ feat_c + (1|subj_id),
                                    family = binomial, data = cue_first_nomask)
summary(feat_type_error_mod_nomask)
confint(feat_type_error_mod_nomask)

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
