devtools::load_all("propertyverification")
data(property_verification)

library(lme4)
library(dplyr)


# Create contrast variables
# -------------------------
property_verification <- recode_mask_type(property_verification)
property_verification <- recode_feat_type(property_verification)
property_verification <- recode_exp(property_verification)

# Drop outlier subjects
# ---------------------
source("scripts/outliers.R")
property_verification <- filter(property_verification,
                                subj_id %nin% cue_first_outliers,
                                subj_id %nin% question_first_outliers)

# Visualize the effect
# --------------------
property_verification %>% group_by(exp, feat_type, mask_type) %>%
  summarize(
    rt = mean(rt, na.rm = TRUE),
    error_rate = mean(is_error, na.rm = TRUE))

library(ggplot2)
ggplot(property_verification, aes(x = feat_type, y = rt, fill = mask_type)) +
  stat_summary(fun.y = "mean", geom = "bar", position = "dodge") +
  facet_wrap("exp", nrow = 1)

# Predict reaction times based on mask_type, cue_type, and experiment
# -------------------------------------------------------------------
rt_mod <- lmer(rt ~ mask_c + feat_c + mask_c:feat_c + exp_c + mask_c:feat_c:exp_c + (1|subj_id), data = property_verification)
summary(rt_mod)
