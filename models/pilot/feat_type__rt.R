devtools::load_all("propertyverification")
data(cue_first)

library(lme4)
library(dplyr)


# Create contrast variables
# -------------------------
cue_first <- recode_mask_type(cue_first)
cue_first <- recode_feat_type(cue_first)

# Drop outlier subject
# --------------------
cue_first <- filter(cue_first, subj_id != cue_first_outliers)

# Models prediction RT
# --------------------
# Feature type on nomask trials
baseline_rt_mod <- lmerTest::lmer(rt ~ feat_c + (1|subj_id),
                                  data = filter(cue_first, mask_type == "nomask"))
summary(baseline_rt_mod)
report_lmerTest_effect(baseline_rt_mod, "feat_c")
# -11.89 ms., 95% CI [-26.79, 3.01], p = 0.1180


# Predict reaction times from mask_type and cue_type
rt_mod <- lmerTest::lmer(rt ~ mask_c * feat_c + (1|subj_id), data = cue_first)
summary(rt_mod)
confint(rt_mod)


# Predict RT from feature type on nomask trials only
rt_mod_nomask <- lmerTest::lmer(rt ~ feat_c + (1|subj_id), data = filter(cue_first, mask_type == "nomask"))
summary(rt_mod_nomask)
confint(rt_mod_nomask)


# RT by mask for visual questions only
cue_first_visual <- filter(cue_first, feat_type == "visual")
rt_mod_visual <- lmerTest::lmer(rt ~ mask_c + (1|subj_id), data = cue_first_visual)
summary(rt_mod_visual)

cue_first_nonvisual <- filter(cue_first, feat_type == "nonvisual")
rt_mod_nonvisual <- lmerTest::lmer(rt ~ mask_c + (1|subj_id), data = cue_first_nonvisual)
summary(rt_mod_nonvisual)
