source("scripts/all_data.R")

library(lme4)
library(dplyr)

source("scripts/contrasts.R")

# Create contrast variables
# -------------------------
property_verification <- recode_mask_type(property_verification)
property_verification <- recode_feat_type(property_verification)
property_verification <- recode_exp(property_verification)

# Drop outlier subjects
# ---------------------
source("scripts/outliers.R")
property_verification <- filter(property_verification,
                                subj_id != cue_first_outliers,
                                subj_id != question_first_outliers)

# Count the number of subjects in each experiment that show the effect
library(reshape2)
mean_error_rates <- property_verification %>%
  group_by(exp, subj_id, mask_type, feat_type) %>%
  summarize(error_rate = mean(is_error, na.rm = TRUE)) %>%
  dcast(exp + subj_id ~ feat_type + mask_type, value.var = "error_rate") %>%
  transmute(
    exp, subj_id,
    effect_of_mask_visual = visual_nomask - visual_mask,
    effect_of_mask_nonvisual = nonvisual_nomask - visual_nomask) %>%
  melt(id.vars = c("exp", "subj_id"), variable.name = "effect", value.name = "diff_in_error")

mean_error_rates %>%
  group_by(exp) %>%
  summarize(
    n = length(unique(subj_id)),
    n_effect_of_mask_visual = sum(diff_in_error[effect == "effect_of_mask_visual"] < 0.0),
    prop_effect_of_mask_visual = n_effect_of_mask_visual/n,
    n_interaction = sum(diff_in_error[effect == "effect_of_mask_visual"] < diff_in_error[effect == "effect_of_mask_nonvisual"]),
    prop_interaction = n_interaction/n
  )

# In experiment 1A (question_first), 15 of the 24 subjects had higher error
# rates on the visual questions with interference than they did on the visual
# questions without interference, so 62.5% showed the effect. In experiment 1B
# (cue_first), 26 of the 45 subjects (57.8%) showed the same effect. So the
# proportion of subjects showing the effect is pretty much the same.

# Measure the size of the effect in each experiment
mean_error_rates %>%
  group_by(exp) %>%
  summarize(
    ave_effect_of_mask_visual = mean(diff_in_error[effect == "effect_of_mask_visual"])
  )

# The size of the effect is where the difference is. For experiment 1A
# (question_first), people's error rates on visual questions was 3.3% higher
# when there was interference. For experiment 1B, people's error rate was only
# 1.6% higher.