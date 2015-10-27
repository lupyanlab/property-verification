devtools::load_all("propertyverification")
data(question_first)

library(dplyr)
library(lme4)

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

# ------------------------------------------------------------------------------
# Response to Reviewers

# Get average RT on nomask trials for each question by id
question_difficulties <- question_first %>%
  filter(mask_type == "nomask") %>%
  group_by(question_id) %>%
  summarize(mean_nomask_rt = mean(rt, na.rm = TRUE))

# Merge the mean_nomask_rt column back in the data
question_first <- left_join(question_first, question_difficulties)

# Fit the model, controlling for mean_nomask_rt
feat_type_error_rtcovariate_mod <- glmer(is_error ~ mask_c * feat_c + mean_nomask_rt + (1|subj_id),
                 family = binomial,
                 data = question_first)
summary(feat_type_error_rtcovariate_mod)$coefficients
#> mean_nomask_rt = 0.0028, z = 10, p < 0.001
#> mask_c:feat_c = 0.55, z = 2.54, p = 0.01

# mean_nomask_rt effect means that questions that took longer to respond to
# were indeed more difficult. This is a sanity check.
# However, controlling for the effect of question difficulty as measured by
# RT on nomask trials, the key interaction (mask_c:feat_c) was still reliable.
