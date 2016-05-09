library(dplyr)
library(lme4)

library(propertyverificationdata)
data("question_first")

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_outliers %>%
  filter(is_outlier == 0)

rt_mask <- lmerTest::lmer(rt ~ mask_c + (1|subj_id),
                data = filter(question_first, exp_run != 4))
summary(rt_mask)

report_lmerTest_effect(rt_mask, "mask_c")
# -13.36 ms., 95% CI [-21.66, -5.06], p = 0.0016