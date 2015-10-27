source("scripts/question_first_data.R")

library(lme4)

source("scripts/contrasts.R")
source("scripts/outliers.R")
source("scripts/report_stats.R")

# Create contrast variables
# -------------------------
question_first <- recode_mask_type(question_first)
question_first <- recode_feat_type(question_first)

# Drop outlier subjects
# ---------------------
question_first <- filter(question_first, subj_id != question_first_outliers)

# Models predicting RT
# --------------------
# Predict RT from feature type on nomask trials only
rt_mod_nomask <- lmerTest::lmer(rt ~ feat_c + (1|subj_id), data = filter(question_first, mask_type == "nomask"))
summary(rt_mod_nomask)
confint(rt_mod_nomask)
report_lmerTest_effect(rt_mod_nomask, "feat_c")

# Predict reaction times from mask_type and cue_type
rt_mod <- lmerTest::lmer(rt ~ mask_c * feat_c + (1|subj_id), data = question_first)
summary(rt_mod)
confint(rt_mod)
report_lmerTest_effect(rt_mod, "mask_c:feat_c")

# RT by mask for visual questions only
question_first_visual <- filter(question_first, feat_type == "visual")
rt_mod_visual <- lmerTest::lmer(rt ~ mask_c + (1|subj_id), data = question_first_visual)
summary(rt_mod_visual)

# RT by mask for nonvisual question only
question_first_nonvisual <- filter(question_first, feat_type == "nonvisual")
rt_mod_nonvisual <- lmerTest::lmer(rt ~ mask_c + (1|subj_id), data = question_first_nonvisual)
summary(rt_mod_nonvisual)

# ------------------------------------------------------------------------------
# Response to Reviewers

# Create new variables to test main effects with other conditions
# as covariates.
question_first <- question_first %>%
  mutate(
    feat_visual = ifelse(feat_type == "visual", 0, 1),
    # make visual trials negative so params are same sign as feat_visual
    feat_nonvisual = ifelse(feat_type == "nonvisual", 0, -1),

    mask_nomask = ifelse(mask_type == "nomask", 0, 1),
    # make masked trials negative so params as same sign as mask_nomask
    mask_mask = ifelse(mask_type == "mask", 0, -1)
  )

# Here is the model the reviewer is concerned with:
# the marginal difference in RTs on visual and nonvisual trials 
# such that RTs are slower to visual feature questions.
lmerTest::lmer(rt ~ feat_c + (1|subj_id),
               data = filter(question_first, mask_type == "nomask")) %>%
  tidy(effects = "fixed")
#> feat_c = 18 ms, t = 1.7

# Here is the same effect (feat_c on nomask trials), but in a model 
# including RTs on masked trials as a covariate.
lmerTest::lmer(rt ~ feat_c * mask_nomask + (1|subj_id),
               data = question_first) %>%
  tidy(effects = "fixed")
#> feat_c = 18 ms, t = 1.7, p = 0.08

# What about the difference between visual and nonvisual feature questions on
# masked trials? On the mask trials RTs on visual feature questions are slower
# than RTs on nonvisual feature questions, controlling for the difference
# between visual feature trials and nonvisual feature trials on the nomask
# trials.
lmerTest::lmer(rt ~ feat_c * mask_mask + (1|subj_id),
               data = question_first) %>%
  tidy(effects = "fixed")
#> feat_c = 36 ms, t = 3.4, p < 0.01

# This effect is expected: if the mask makes the visual questions harder,
# RTs should be longer. But we want to account for the baseline difference
# between visual and nonvisual questions, which is why we care about the
# interaction more than any of the main effects. And the interaction is
# not reliable, so we can't reject the null.

# Basically, the reviewer's critique is only valid if we our main conclusions
# are drawn from models without the interaction term. But they are not. 
# *It is not statistically necessary for visual and nonvisual questions
# to have the same RTs or the same accuracies on the no mask trials in order
# to accurately interpret the effect of the mask on visual and nonvisual trials.*

# No effect of mask on visual feature trials,
# including the effect of the mask on nonvisual feature trials
# as a covariate.
lmerTest::lmer(rt ~ mask_c * feat_visual + (1|subj_id),
               data = question_first
               ) %>% tidy(effects = "fixed")
#> mask_c = -6 ms, t = -0.6

# Significant effect of mask on nonvisual feature trials,
# including the effect of the mask on visual feature trials
# as a covariate.
lmerTest::lmer(rt ~ mask_c * feat_nonvisual + (1|subj_id),
               data = question_first
               ) %>% tidy(effects = "fixed")
#> mask_c = -24 ms, t = -2.3, p = 0.02
# N.B. the sign of the effect. It means the mask improved performance.


