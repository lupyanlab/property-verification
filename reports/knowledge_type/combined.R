library(dplyr)
library(lme4)

library(propertyverificationdata)
data("question_first")

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_outliers %>%
  filter(is_outlier == 0, exp_run %in% c(1, 4))

error_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id),
                   family = "binomial", data = question_first)
report_glmer_effect(error_mod, "feat_c:mask_c")