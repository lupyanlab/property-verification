library(dplyr)
library(lme4)

library(propertyverificationdata)
data("question_first")

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_outliers %>%
  filter(is_outlier == 0) %>%
  mutate(response_window = ifelse(exp_run == 4, "during_mask", "after_mask"))

rt_mask <- lmerTest::lmer(rt ~ mask_c + (1|subj_id),
                data = filter(question_first, response_window == "after_mask"))
summary(rt_mask)
report_lmerTest_effect(rt_mask, "mask_c")
# -13.36 ms., 95% CI [-21.66, -5.06], z = -3.2, p = 0.0016

rt_mask <- lmerTest::lmer(rt ~ mask_c + (1|subj_id),
                          data = filter(question_first, response_window == "during_mask"))
summary(rt_mask)
report_lmerTest_effect(rt_mask, "mask_c")
# -32.74 ms., 95% CI [-43, 22], z = -6.0, p < -0.001

rt_mask <- lmerTest::lmer(rt ~ mask_c * + (1|subj_id) + (1|response_window),
                          data = question_first)
summary(rt_mask)
report_lmerTest_effect(rt_mask, "mask_c")
# -19.10 ms., 95% CI [-26, -12], p = 1.84e-08

feat_mask <- lmerTest::lmer(rt ~ mask_c * feat_c + (1|subj_id) + (1|response_window),
                            data = question_first)
summary(feat_mask)
report_lmerTest_effect(feat_mask, "feat_c")
# feat_c 25.11 ms., 95% CI [18.48, 31.75], p = 1.25e-13
report_lmerTest_effect(feat_mask, "mask_c:feat_c")
# mask_c:feat_c 0.87 ms., 95% CI [, ], p = 0.89

feat <- lmerTest::lmer(rt ~ feat_c + (1|subj_id) + (1|response_window),
                       data = filter(question_first, mask_type == "nomask"))
summary(feat)

error <- glmer(is_error ~ feat_c + (1|subj_id), family = "binomial",
               data = filter(question_first, mask_type == "nomask"))
summary(error)
