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
property_verification <- filter(property_verification,
                                subj_id %nin% cue_first_outliers,
                                subj_id %nin% question_first_outliers)

# Descriptives
property_verification %>% group_by(feat_type, mask_type) %>%
  summarize(
    rt = mean(rt, na.rm = TRUE),
    error_rate = mean(is_error, na.rm = TRUE) * 100 %>% round(digits = 2))

# Visualize the effect
library(ggplot2)
ggplot(property_verification, aes(x = feat_type, y = is_error, fill = mask_type)) +
  stat_summary(fun.y = "mean", geom = "bar", position = "dodge") +
  facet_wrap("exp", nrow = 1)

# Models predicting error rate
# ----------------------------
# Visual versus nonvisual on nomask trials
nomask_mod <- glmer(is_error ~ feat_c + (1|subj_id),
                    family = binomial,
                    data = filter(property_verification, mask_type == "nomask"))
summary(nomask_mod)
report_glmer_effect(nomask_mod, "feat_c")
# 0.03 log-odds, 95% CI [-0.12, 0.19], p = 0.6624


# Effect of mask on visual questions only
mask_mod_visual <- glmer(is_error ~ mask_c + exp_c + (1|subj_id),
                         family = binomial,
                         data = filter(property_verification, feat_type == "visual"))
summary(mask_mod_visual)
report_glmer_effect(mask_mod_visual, "mask_c")
# 0.21 log-odds, 95% CI [0.06, 0.36], p = 0.0050


# Effect of mask on nonvisual questions only
mask_mod_nonvisual <- glmer(is_error ~ mask_c + exp_c + (1|subj_id),
                            family = binomial,
                            data = filter(property_verification, feat_type == "nonvisual"))
summary(mask_mod_nonvisual)
report_glmer_effect(mask_mod_nonvisual, "mask_c")
# 0.00 log-odds, 95% CI [-0.16, 0.16], p = 0.9870


# Interaction between mask and feature type
error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                   family = binomial, data = property_verification)
summary(error_mod)
report_glmer_effect(error_mod, "mask_c:feat_c")
# 

# Interaction between mask and feature type BY EXPERIMENT
error_mod <- glmer(is_error ~ mask_c + feat_c + mask_c:feat_c + exp_c + mask_c:feat_c:exp_c + (1|subj_id),
                   family = binomial, data = property_verification)
summary(error_mod)
report_glmer_effect(error_mod, "mask_c:feat_c")
# 0.31 log-odds, 95% CI [0.07, 0.55], p = 0.0120
report_glmer_effect(error_mod, "mask_c:feat_c:exp_c")
# 0.43 log-odds, 95% CI [-0.05, 0.92], p = 0.0758


