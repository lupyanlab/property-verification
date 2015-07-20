source("scripts/question_first_data.R")

library(lme4)

source("scripts/contrasts.R")

# Create contrast variables
# -------------------------
question_first <- recode_mask_type(question_first)
question_first <- recode_feat_type(question_first)

# Drop outlier subjects
# ---------------------
source("scripts/outliers.R")
question_first <- filter(question_first, subj_id != question_first_outliers)

# Predict reaction times from mask_type and cue_type
# --------------------------------------------------
rt_mod <- lmerTest::lmer(rt ~ mask_c * feat_c + (1|subj_id), data = question_first)
summary(rt_mod)
confint(rt_mod)

# Predict RT from feature type on nomask trials only
rt_mod_nomask <- lmerTest::lmer(rt ~ feat_c + (1|subj_id), data = filter(question_first, mask_type == "nomask"))
summary(rt_mod_nomask)
confint(rt_mod_nomask)

# RT by mask for visual questions only
question_first_visual <- filter(question_first, feat_type == "visual")
rt_mod_visual <- lmerTest::lmer(rt ~ mask_c + (1|subj_id), data = question_first_visual)
summary(rt_mod_visual)

question_first_nonvisual <- filter(question_first, feat_type == "nonvisual")
rt_mod_nonvisual <- lmerTest::lmer(rt ~ mask_c + (1|subj_id), data = question_first_nonvisual)
summary(rt_mod_nonvisual)
