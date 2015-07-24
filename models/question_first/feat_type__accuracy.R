CLEAR_GLOBAL_ENVIRONMENT <- FALSE
source("scripts/question_first_data.R")

library(lme4)

source("scripts/contrasts.R")
source("scripts/outliers.R")
source("scripts/report_stats.R")

# Create contrast variables
# -------------------------
question_first <- recode_mask_type(question_first)
question_first <- recode_feat_type(question_first)

# Drop outlier subjects
# ---------------------
question_first <- filter(question_first, subj_id %nin% question_first_outliers)

# Models predicting error rate
# ----------------------------

# Predict error rate from feature type on nomask trials
question_first_nomask <- filter(question_first, mask_type == "nomask")
feat_type_error_mod_nomask <- glmer(is_error ~ feat_c + (1|subj_id),
                                    family = binomial, data = question_first_nomask)
summary(feat_type_error_mod_nomask)
confint(feat_type_error_mod_nomask)
report_glmer_effect(feat_type_error_mod_nomask, "feat_c")

# Feature type (visual or nonvisual)
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = question_first)
summary(feat_type_error_mod)

# 
mask_mod_visual <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial,
                         data = filter(question_first, feat_type == "visual"))
summary(mask_mod_visual)
report_glmer_effect(mask_mod_visual, "mask_c")
# 