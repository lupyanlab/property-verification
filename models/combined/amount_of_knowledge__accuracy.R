devtools::load_all("propertyverification")
data(property_verification)

library(lme4)
library(dplyr)


# Create contrast variables
# -------------------------
property_verification <- recode_mask_type(property_verification)
property_verification <- recode_exp(property_verification)

# Drop outlier subjects
# ---------------------
property_verification <- filter(property_verification,
                                subj_id %nin% question_first_outliers,
                                subj_id %nin% cue_first_outliers)


# Imagery by mask
imagery_mod <- glmer(is_error ~ mask_c * imagery_z + (1|subj_id),
                     family = binomial, data = property_verification)
summary(imagery_mod)
report_glmer_effect(imagery_mod, "mask_c:imagery_z")
# 0.12 log-odds, 95% CI [0.01, 0.23], p = 0.0386


# Facts by mask
facts_mod <- glmer(is_error ~ mask_c * facts_z + (1|subj_id),
                   family = binomial, data = property_verification)
summary(facts_mod)
report_glmer_effect(facts_mod, "mask_c:facts_z")
# -0.05 log-odds, 95% CI [-0.15, 0.06], p = 0.3905


# Difficulty by mask
diff_mod <- glmer(is_error ~ mask_c * diff_z + (1|subj_id),
                  family = binomial, data = property_verification)
summary(diff_mod)
report_glmer_effect(diff_mod, "mask_c:diff_z")
# 0.01 log-odds, 95% CI [-0.09, 0.11], p = 0.9149


# Imagery by mask
# controlling for: diff_z
imagery_mod_diff <- glmer(is_error ~ mask_c * imagery_z + diff_z + (1|subj_id),
                          family = binomial, data = property_verification)
summary(imagery_mod_diff)
report_glmer_effect(imagery_mod_diff, "mask_c:imagery_z")
# 0.10 log-odds, 95% CI [-0.01, 0.22], p = 0.0760


# By experiment
# -------------

# Imagery by mask by experiment
imagery_mod_exp <- glmer(is_error ~ mask_c + imagery_z + mask_c:imagery_z + exp_c + mask_c:imagery_z:exp_c + (1|subj_id),
                         family = binomial, data = property_verification)
summary(imagery_mod_exp)
report_glmer_effect(imagery_mod_exp, "mask_c:imagery_z:exp_c")
# 0.17 log-odds, 95% CI [-0.07, 0.41], p = 0.1692


# Facts by mask by experiment
facts_mod_exp <- glmer(is_error ~ mask_c + facts_z + mask_c:facts_z + exp_c + mask_c:facts_z:exp_c + (1|subj_id),
                       family = binomial, data = property_verification)
summary(facts_mod_exp)
report_glmer_effect(facts_mod_exp, "mask_c:facts_z:exp_c")
# -0.10 log-odds, 95% CI [-0.32, 0.12], p = 0.3805


# Diff by mask by experiment
diff_mod_exp <- glmer(is_error ~ mask_c + diff_z + mask_c:diff_z + exp_c + mask_c:diff_z:exp_c + (1|subj_id),
                      family = binomial, data = property_verification)
summary(diff_mod_exp)
report_glmer_effect(diff_mod_exp, "mask_c:diff_z:exp_c")
# -0.04 log-odds, 95% CI [-0.25, 0.16], p = 0.6717

