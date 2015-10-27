# ---- feat_type__accuracy ----
devtools::load_all("propertyverification")
data(cue_first)

library(lme4)
library(dplyr)

# Remove outlier subjects
cue_first <- filter(cue_first, subj_id %nin% cue_first_outliers)

# Create contrast variables
cue_first <- recode_mask_type(cue_first)
cue_first <- recode_feat_type(cue_first)

# Descriptives
cue_first %>% group_by(feat_type, mask_type) %>%
  summarize(error_rate = mean(is_error, na.rm = TRUE) * 100 %>% round(digits = 2),
            rt = mean(rt, na.rm = TRUE))

# Models predicting error rate
# Baseline differences between feature types
baseline_feat_type_mod <- glmer(is_error ~ feat_c + (1|subj_id),
                                family = binomial,
                                data = filter(cue_first, mask_type == "nomask"))
summary(baseline_feat_type_mod)
report_glmer_effect(baseline_feat_type_mod, "feat_c")
# 0.03 log-odds, 95% CI [-0.15, 0.22], p = 0.7355


# Interference for visual questions only
feat_type_error_mod_visual <- glmer(is_error ~ mask_c + (1|subj_id),
                                    family = binomial,
                                    data = filter(cue_first, feat_type == "visual"))
report_glmer_effect(feat_type_error_mod_visual, "mask_c")
# 0.15 log-odds, 95% CI [-0.03, 0.33], p = 0.1078


# Interference for nonvisual questions only
feat_type_error_mod_nonvisual <- glmer(is_error ~ mask_c + (1|subj_id),
                                       family = binomial,
                                       data = filter(cue_first, feat_type == "nonvisual"))
report_glmer_effect(feat_type_error_mod_nonvisual, "mask_c")
# 0.05 log-odds, 95% CI [-0.13, 0.24], p = 0.5789


# Interference by feature type (visual or nonvisual)
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = cue_first)
summary(feat_type_error_mod)
report_glmer_effect(feat_type_error_mod, "mask_c:feat_c")
# 0.10 log-odds, 95% CI [-0.16, 0.36], p = 0.46

